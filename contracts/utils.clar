;; utils.clar
;; Utility functions and traits for the DAO Unifier protocol

;; Define the fungible token trait
(define-trait ft-trait
  (
    ;; Transfer from the caller to a new principal
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))

    ;; Get the token balance of a specified principal
    (get-balance (principal) (response uint uint))

    ;; Get the total supply of the token
    (get-total-supply () (response uint uint))

    ;; Get the token decimals
    (get-decimals () (response uint uint))

    ;; Get the token name
    (get-name () (response (string-ascii 32) uint))

    ;; Get the token symbol
    (get-symbol () (response (string-ascii 32) uint))
  )
)

;; Define the DAO trait
(define-trait dao-trait
  (
    (get-name () (response (string-ascii 64) uint))
    (get-token () (response principal uint))
    (get-next-proposal-id () (response uint uint))
    (register-proposal (principal) (response uint uint))
    (get-proposal (uint) (response principal uint))
  )
)

;; Helper function to get an ft-trait from a principal
(define-read-only (get-ft-trait (token-contract principal))
  ;; In a real implementation, we would call the token contract to get its trait
  ;; For now, we'll just cast the principal to the trait
  (as-contract token-contract)
)
