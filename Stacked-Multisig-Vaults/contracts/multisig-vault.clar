;; Stacked Multisig Vaults
;; Multi-signature wallet for sBTC and Stacks assets with advanced features

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-signed (err u103))
(define-constant err-insufficient-signatures (err u104))
(define-constant err-transaction-executed (err u105))
(define-constant err-invalid-threshold (err u106))
(define-constant err-vault-inactive (err u107))
(define-constant err-invalid-amount (err u108))
(define-constant err-time-lock-active (err u109))
(define-constant err-invalid-recovery-data (err u110))

(define-map vaults
  uint
  {
    owners: (list 10 principal),
    threshold: uint,
    balance: uint,
    created-at: uint,
    time-delay: uint,
    active: bool,
    daily-limit: uint,
    daily-spent: uint,
    last-reset: uint,
    recovery-address: (optional principal),
    recovery-delay: uint
  })

(define-map pending-transactions
  uint
  {
    vault-id: uint,
    recipient: principal,
    amount: uint,
    signatures: (list 10 principal),
    signature-count: uint,
    created-at: uint,
    execute-after: uint,
    executed: bool,
    cancelled: bool,
    description: (string-ascii 256),
    transaction-type: (string-ascii 20)
  })

(define-map vault-ownership
  { vault-id: uint, owner: principal }
  bool)

(define-map recovery-requests
  uint
  {
    vault-id: uint,
    new-owners: (list 10 principal),
    new-threshold: uint,
    initiated-by: principal,
    initiated-at: uint,
    execute-after: uint,
    executed: bool
  })

(define-data-var next-vault-id uint u1)
(define-data-var next-transaction-id uint u1)
(define-data-var next-recovery-id uint u1)

(define-public (create-vault 
  (owners (list 10 principal))
  (threshold uint)
  (time-delay uint)
  (daily-limit uint)
  (recovery-address (optional principal))
  (recovery-delay uint))
  (let ((caller tx-sender)
        (vault-id (var-get next-vault-id))
        (owner-count (len owners)))
    (asserts! (> threshold u0) err-invalid-threshold)
    (asserts! (<= threshold owner-count) err-invalid-threshold)
    (asserts! (> owner-count u0) err-invalid-threshold)
    (asserts! (> daily-limit u0) err-invalid-amount)
    
    (map-set vaults vault-id {
      owners: owners,
      threshold: threshold,
      balance: u0,
      created-at: block-height,
      time-delay: time-delay,
      active: true,
      daily-limit: daily-limit,
      daily-spent: u0,
      last-reset: block-height,
      recovery-address: recovery-address,
      recovery-delay: recovery-delay
    })
    
    (fold set-vault-owner owners vault-id)
    (var-set next-vault-id (+ vault-id u1))
    (ok vault-id)))

(define-private (set-vault-owner (owner principal) (vault-id uint))
  (begin
    (map-set vault-ownership {vault-id: vault-id, owner: owner} true)
    vault-id))

(define-public (deposit-to-vault (vault-id uint) (amount uint))
  (let ((caller tx-sender)
        (vault (unwrap! (map-get? vaults vault-id) err-not-found)))
    (asserts! (get active vault) err-vault-inactive)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= (stx-get-balance caller) amount) err-unauthorized)
    
    (try! (stx-transfer? amount caller (as-contract tx-sender)))
    
    (ok (map-set vaults vault-id
      (merge vault {balance: (+ (get balance vault) amount)})))))

(define-public (update-vault-settings
  (vault-id uint)
  (new-threshold uint)
  (new-daily-limit uint)
  (new-time-delay uint))
  (let ((caller tx-sender)
        (vault (unwrap! (map-get? vaults vault-id) err-not-found)))
    (asserts! (get active vault) err-vault-inactive)
    (asserts! (default-to false (map-get? vault-ownership {vault-id: vault-id, owner: caller})) err-unauthorized)
    (asserts! (> new-threshold u0) err-invalid-threshold)
    (asserts! (<= new-threshold (len (get owners vault))) err-invalid-threshold)
    (asserts! (> new-daily-limit u0) err-invalid-amount)
    
    (ok (map-set vaults vault-id
      (merge vault {
        threshold: new-threshold,
        daily-limit: new-daily-limit,
        time-delay: new-time-delay
      })))))

(define-public (propose-transaction 
  (vault-id uint)
  (recipient principal)
  (amount uint)
  (description (string-ascii 256))
  (transaction-type (string-ascii 20)))
  (let ((caller tx-sender)
        (vault (unwrap! (map-get? vaults vault-id) err-not-found))
        (transaction-id (var-get next-transaction-id)))
    (asserts! (get active vault) err-vault-inactive)
    (asserts! (default-to false (map-get? vault-ownership {vault-id: vault-id, owner: caller})) err-unauthorized)
    (asserts! (>= (get balance vault) amount) err-invalid-amount)
    (asserts! (> amount u0) err-invalid-amount)
    
    (map-set pending-transactions transaction-id {
      vault-id: vault-id,
      recipient: recipient,
      amount: amount,
      signatures: (list caller),
      signature-count: u1,
      created-at: block-height,
      execute-after: (+ block-height (get time-delay vault)),
      executed: false,
      cancelled: false,
      description: description,
      transaction-type: transaction-type
    })
    
    (var-set next-transaction-id (+ transaction-id u1))
    (ok transaction-id)))

(define-public (sign-transaction (transaction-id uint))
  (let ((caller tx-sender)
        (transaction (unwrap! (map-get? pending-transactions transaction-id) err-not-found))
        (vault (unwrap! (map-get? vaults (get vault-id transaction)) err-not-found)))
    (asserts! (get active vault) err-vault-inactive)
    (asserts! (not (get executed transaction)) err-transaction-executed)
    (asserts! (not (get cancelled transaction)) err-transaction-executed)
    (asserts! (default-to false (map-get? vault-ownership {vault-id: (get vault-id transaction), owner: caller})) err-unauthorized)
    (asserts! (is-none (index-of (get signatures transaction) caller)) err-already-signed)
    
    (let ((new-signatures (unwrap! (as-max-len? (append (get signatures transaction) caller) u10) err-unauthorized))
          (new-count (+ (get signature-count transaction) u1)))
      
      (ok (map-set pending-transactions transaction-id
        (merge transaction {
          signatures: new-signatures,
          signature-count: new-count
        }))))))

(define-public (execute-transaction (transaction-id uint))
  (let ((caller tx-sender)
        (transaction (unwrap! (map-get? pending-transactions transaction-id) err-not-found))
        (vault (unwrap! (map-get? vaults (get vault-id transaction)) err-not-found))
        (vault-with-reset (reset-daily-limit-if-needed vault)))
    (asserts! (get active vault-with-reset) err-vault-inactive)
    (asserts! (not (get executed transaction)) err-transaction-executed)
    (asserts! (not (get cancelled transaction)) err-transaction-executed)
    (asserts! (>= (get signature-count transaction) (get threshold vault-with-reset)) err-insufficient-signatures)
    (asserts! (>= block-height (get execute-after transaction)) err-time-lock-active)
    (asserts! (>= (get balance vault-with-reset) (get amount transaction)) err-invalid-amount)
    
    ;; Check daily limit for regular transactions
    (if (is-eq (get transaction-type transaction) "regular")
        (asserts! (<= (+ (get daily-spent vault-with-reset) (get amount transaction)) (get daily-limit vault-with-reset)) err-unauthorized)
        true)
    
    (try! (as-contract (stx-transfer? (get amount transaction) tx-sender (get recipient transaction))))
    
    (map-set pending-transactions transaction-id
      (merge transaction {executed: true}))
    
    (let ((updated-vault (merge vault-with-reset {
            balance: (- (get balance vault-with-reset) (get amount transaction)),
            daily-spent: (if (is-eq (get transaction-type transaction) "regular")
                           (+ (get daily-spent vault-with-reset) (get amount transaction))
                           (get daily-spent vault-with-reset))
          })))
      (ok (map-set vaults (get vault-id transaction) updated-vault)))))

(define-public (cancel-transaction (transaction-id uint))
  (let ((caller tx-sender)
        (transaction (unwrap! (map-get? pending-transactions transaction-id) err-not-found))
        (vault (unwrap! (map-get? vaults (get vault-id transaction)) err-not-found)))
    (asserts! (get active vault) err-vault-inactive)
    (asserts! (not (get executed transaction)) err-transaction-executed)
    (asserts! (not (get cancelled transaction)) err-transaction-executed)
    (asserts! (default-to false (map-get? vault-ownership {vault-id: (get vault-id transaction), owner: caller})) err-unauthorized)
    
    (ok (map-set pending-transactions transaction-id
      (merge transaction {cancelled: true})))))
