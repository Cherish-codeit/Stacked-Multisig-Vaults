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
