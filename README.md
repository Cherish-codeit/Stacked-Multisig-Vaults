# Stacked Multisig Vaults

Advanced multi-signature wallets for sBTC and Stacks assets featuring customizable security parameters and time-delayed execution.

## Features

- **Flexible Multisig**: Support for M-of-N signature schemes up to 10 owners
- **Time-Delayed Security**: Configurable execution delays for transaction safety
- **Proposal System**: Structured workflow for transaction approval
- **Signature Management**: Add and revoke signatures before execution
- **Emergency Controls**: Admin pause functionality for security incidents
- **Audit Trail**: Complete transaction history and signature tracking
- **Access Control**: Granular ownership verification and permissions

## Contract Functions

### Public Functions
- `create-vault()`: Initialize new multisig vault with owners and threshold
- `deposit-to-vault()`: Add funds to vault balance
- `propose-transaction()`: Create new transaction proposal
- `sign-transaction()`: Add signature to pending transaction
- `execute-transaction()`: Execute fully-signed transaction after delay
- `revoke-signature()`: Remove signature from pending transaction

### Read-Only Functions
- `get-vault-info()`: Retrieve vault configuration and balance
- `get-transaction-info()`: View transaction details and signatures
- `is-vault-owner()`: Check ownership status
- `get-required-signatures()`: Get signature threshold for vault

## Usage

Create vaults with multiple owners, propose transactions, collect required signatures, wait for time delay, then execute approved transactions.

## Security Model

Combines multi-signature requirements with time delays to provide robust protection against unauthorized transactions and compromised keys.