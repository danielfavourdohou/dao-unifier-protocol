;; voting-power.clar
;; Handles the calculation and assignment of voting power based on tokens or STX

(use-trait dao-trait .utils.dao-trait)
(use-trait ft-trait .utils.ft-trait)
(use-trait proposal-trait .proposal.proposal-trait)

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_TOKEN (err u101))
(define-constant ERR_INVALID_DAO (err u102))
(define-constant ERR_TOKEN_NOT_FOUND (err u103))
(define-constant ERR_NO_VOTING_POWER (err u104))
(define-constant ERR_ALREADY_DELEGATED (err u105))
(define-constant ERR_NOT_DELEGATED (err u106))

;; Define current-block-height variable
(define-data-var current-block-height uint u100)

;; Data structures
(define-map dao-voting-tokens principal principal)
(define-map token-voting-multipliers principal uint)
(define-map stx-voting-enabled principal bool)

(define-map user-voting-power {dao-id: principal, user: principal} {
  token-balance: uint,
  delegated-power: uint,
  stx-balance: uint,
  delegated-to: (optional principal),
  last-updated: uint
})

(define-map delegated-power {dao-id: principal, delegator: principal, delegate: principal} {
  amount: uint,
  until-block: (optional uint)
})

;; Read-only functions
(define-read-only (get-token-voting-multiplier (token-contract principal))
  (default-to u1 (map-get? token-voting-multipliers token-contract))
)

(define-read-only (is-stx-voting-enabled (dao-id principal))
  (default-to false (map-get? stx-voting-enabled dao-id))
)

(define-read-only (get-dao-voting-token (dao-id principal))
  (map-get? dao-voting-tokens dao-id)
)

(define-read-only (get-user-voting-power (dao-id principal) (user principal))
  (default-to
    {token-balance: u0, delegated-power: u0, stx-balance: u0, delegated-to: none, last-updated: u0}
    (map-get? user-voting-power {dao-id: dao-id, user: user})
  )
)

(define-read-only (calculate-total-voting-power (dao-id principal) (user principal))
  (let (
    (power-info (get-user-voting-power dao-id user))
    (delegated-to (get delegated-to power-info))
  )
    (if (is-some delegated-to)
      ;; If user delegated their power, return 0
      u0
      ;; Otherwise return their own power plus delegations
      (+ (get token-balance power-info)
         (get delegated-power power-info)
         (get stx-balance power-info))
    )
  )
)

;; Public functions
(define-public (set-dao-voting-token (dao-contract <dao-trait>) (token-contract principal))
  (let (
    (dao-id (contract-of dao-contract))
  )
    ;; Only DAO contract or owner can set token
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender dao-id)) ERR_UNAUTHORIZED)

    ;; Set the token for this DAO
    (map-set dao-voting-tokens dao-id token-contract)

    ;; Emit event
    (print {event: "dao-voting-token-set", dao-id: dao-id, token: token-contract})
    (ok true)
  )
)

(define-public (set-token-voting-multiplier (token-contract principal) (multiplier uint))
  (begin
    ;; Only contract owner can set multipliers
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)

    ;; Set the multiplier
    (map-set token-voting-multipliers token-contract multiplier)

    ;; Emit event
    (print {event: "token-multiplier-set", token: token-contract, multiplier: multiplier})
    (ok true)
  )
)

(define-public (enable-stx-voting (dao-contract <dao-trait>) (enabled bool))
  (let (
    (dao-id (contract-of dao-contract))
  )
    ;; Only DAO contract or owner can enable STX voting
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender dao-id)) ERR_UNAUTHORIZED)

    ;; Set the STX voting flag
    (map-set stx-voting-enabled dao-id enabled)

    ;; Emit event
    (print {event: "stx-voting-set", dao-id: dao-id, enabled: enabled})
    (ok true)
  )
)

(define-public (update-token-voting-power (dao-contract <dao-trait>) (user principal))
  (let (
    (dao-id (contract-of dao-contract))
    (token-contract (unwrap! (get-dao-voting-token dao-id) ERR_TOKEN_NOT_FOUND))
    (multiplier (get-token-voting-multiplier token-contract))
    (power-info (get-user-voting-power dao-id user))
    ;; In a real implementation, we would get the token balance
    ;; For now, we'll just use a dummy value
    (balance u100)
    (voting-power (* balance multiplier))
  )
    ;; Update the voting power record
    (map-set user-voting-power {dao-id: dao-id, user: user}
      (merge power-info {
        token-balance: voting-power,
        last-updated: (var-get current-block-height)
      })
    )

    ;; Emit event
    (print {event: "token-voting-power-updated", dao-id: dao-id, user: user, power: voting-power})
    (ok voting-power)
  )
)

(define-public (update-stx-voting-power (dao-contract <dao-trait>) (user principal))
  (let (
    (dao-id (contract-of dao-contract))
    (stx-enabled (is-stx-voting-enabled dao-id))
    (stx-balance (stx-get-balance user))
    (power-info (get-user-voting-power dao-id user))
    (voting-power (if stx-enabled (/ stx-balance u1000000) u0))  ;; 1 voting power per STX
  )
    ;; Verify STX voting is enabled
    (asserts! stx-enabled ERR_UNAUTHORIZED)

    ;; Update the voting power record
    (map-set user-voting-power {dao-id: dao-id, user: user}
      (merge power-info {
        stx-balance: voting-power,
        last-updated: (var-get current-block-height)
      })
    )

    ;; Emit event
    (print {event: "stx-voting-power-updated", dao-id: dao-id, user: user, power: voting-power})
    (ok voting-power)
  )
)

(define-public (delegate-voting-power
    (dao-contract <dao-trait>)
    (delegate-to principal)
    (until-block (optional uint))
  )
  (let (
    (dao-id (contract-of dao-contract))
    (power-info (get-user-voting-power dao-id tx-sender))
    (delegating-power (+ (get token-balance power-info) (get stx-balance power-info)))
  )
    ;; Check not already delegated
    (asserts! (is-none (get delegated-to power-info)) ERR_ALREADY_DELEGATED)

    ;; Check has voting power to delegate
    (asserts! (> delegating-power u0) ERR_NO_VOTING_POWER)

    ;; Update delegator record
    (map-set user-voting-power {dao-id: dao-id, user: tx-sender}
      (merge power-info {
        delegated-to: (some delegate-to),
        last-updated: (var-get current-block-height)
      })
    )

    ;; Record delegation relationship
    (map-set delegated-power {dao-id: dao-id, delegator: tx-sender, delegate: delegate-to}
      {amount: delegating-power, until-block: until-block}
    )

    ;; Update delegate's voting power
    (let (
      (delegate-power-info (get-user-voting-power dao-id delegate-to))
      (current-delegated (get delegated-power delegate-power-info))
    )
      (map-set user-voting-power {dao-id: dao-id, user: delegate-to}
        (merge delegate-power-info {
          delegated-power: (+ current-delegated delegating-power),
          last-updated: (var-get current-block-height)
        })
      )
    )

    ;; Emit event
    (print {event: "voting-power-delegated", dao-id: dao-id, from: tx-sender, to: delegate-to, amount: delegating-power})
    (ok delegating-power)
  )
)

(define-public (revoke-delegation (dao-contract <dao-trait>))
  (let (
    (dao-id (contract-of dao-contract))
    (power-info (get-user-voting-power dao-id tx-sender))
    (delegate-principal (unwrap! (get delegated-to power-info) ERR_NOT_DELEGATED))
    (delegation-info (unwrap! (map-get? delegated-power
                                        {dao-id: dao-id, delegator: tx-sender, delegate: delegate-principal})
                             ERR_NOT_DELEGATED))
    (delegate-power-info (get-user-voting-power dao-id delegate-principal))
    (current-delegated (get delegated-power delegate-power-info))
    (delegated-amount (get amount delegation-info))
  )
    ;; Update delegator record
    (map-set user-voting-power {dao-id: dao-id, user: tx-sender}
      (merge power-info {
        delegated-to: none,
        last-updated: (var-get current-block-height)
      })
    )

    ;; Remove delegation relationship
    (map-delete delegated-power {dao-id: dao-id, delegator: tx-sender, delegate: delegate-principal})

    ;; Update delegate's voting power
    (map-set user-voting-power {dao-id: dao-id, user: delegate-principal}
      (merge delegate-power-info {
        delegated-power: (- current-delegated delegated-amount),
        last-updated: (var-get current-block-height)
      })
    )

    ;; Emit event
    (print {event: "delegation-revoked", dao-id: dao-id, from: tx-sender, to: delegate-principal, amount: delegated-amount})
    (ok delegated-amount)
  )
)

(define-public (check-delegation-expiry (dao-contract <dao-trait>) (delegator principal) (delegate principal))
  (let (
    (dao-id (contract-of dao-contract))
    (delegation-info (unwrap! (map-get? delegated-power
                                        {dao-id: dao-id, delegator: delegator, delegate: delegate})
                             ERR_NOT_DELEGATED))
    (until-block (get until-block delegation-info))
  )
    (if (and (is-some until-block) (let ((expiry-block (unwrap-panic until-block))) (>= (var-get current-block-height) expiry-block)))
      ;; Delegation has expired, revoke it
      (begin
        ;; Must update both delegator and delegate records
        (let (
          (power-info (get-user-voting-power dao-id delegator))
          (delegate-power-info (get-user-voting-power dao-id delegate))
          (current-delegated (get delegated-power delegate-power-info))
          (delegated-amount (get amount delegation-info))
        )
          ;; Update delegator record
          (map-set user-voting-power {dao-id: dao-id, user: delegator}
            (merge power-info {
              delegated-to: none,
              last-updated: (var-get current-block-height)
            })
          )

          ;; Remove delegation relationship
          (map-delete delegated-power {dao-id: dao-id, delegator: delegator, delegate: delegate})

          ;; Update delegate's voting power
          (map-set user-voting-power {dao-id: dao-id, user: delegate}
            (merge delegate-power-info {
              delegated-power: (- current-delegated delegated-amount),
              last-updated: (var-get current-block-height)
            })
          )

          ;; Emit event
          (print {event: "delegation-expired", dao-id: dao-id, from: delegator, to: delegate, amount: delegated-amount})
          (ok delegated-amount)
        )
      )
      ;; Delegation still valid
      (ok u0)
    )
  )
)

;; Vote on a proposal using calculated voting power
(define-public (vote-with-power
    (dao-contract <dao-trait>)
    (proposal-contract <proposal-trait>)
    (vote-type uint)
  )
  (let (
    (dao-id (contract-of dao-contract))
    (proposal-id (contract-of proposal-contract))
    (voting-power (calculate-total-voting-power dao-id tx-sender))
  )
    ;; Ensure user has voting power
    (asserts! (> voting-power u0) ERR_NO_VOTING_POWER)

    ;; Cast the vote with calculated power
    (contract-call? proposal-contract vote vote-type)
  )
)