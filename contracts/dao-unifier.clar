;; dao-unifier.clar
;; Core aggregator contract that serves as the entry point for the DAO Unifier system

;; Import traits
(use-trait proposal-trait .proposal.proposal-trait)
(use-trait dao-trait .utils.dao-trait)

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_NAME (err u101))
(define-constant ERR_INVALID_TOKEN (err u102))
(define-constant ERR_DAO_NOT_FOUND (err u103))
(define-constant ERR_ALREADY_INITIALIZED (err u104))

;; Define current-block-height variable
(define-data-var current-block-height uint u100)

;; Data structures
(define-map daos principal {
  name: (string-ascii 64),
  token: principal,
  owner: principal,
  proposal-counter: uint,
  created-at: uint
})

(define-map dao-proposals { dao-id: principal, proposal-id: uint } principal)
(define-data-var dao-count uint u0)

;; Read-only functions
(define-read-only (get-dao-details (dao-id principal))
  (map-get? daos dao-id)
)

(define-read-only (get-dao-count)
  (var-get dao-count)
)

;; Public functions
(define-public (create-dao
    (name (string-ascii 64))
    (token-contract principal)
    (dao-contract principal) ;; The principal of the new DAO contract
  )
  (begin
    ;; Validate inputs
    (asserts! (> (len name) u0) ERR_INVALID_NAME)
    (asserts! (is-none (map-get? daos dao-contract)) ERR_ALREADY_INITIALIZED)

    ;; Register the DAO
    (map-set daos dao-contract {
      name: name,
      token: token-contract,
      owner: tx-sender,
      proposal-counter: u0,
      created-at: (var-get current-block-height)
    })

    ;; Increment DAO count
    (var-set dao-count (+ (var-get dao-count) u1))

    ;; Emit creation event
    (print { event: "dao-created", dao-id: dao-contract, name: name, owner: tx-sender })
    (ok dao-contract)
  )
)

(define-public (register-proposal-for-dao
    (dao-id principal)
    (proposal-contract principal)
  )
  (let (
    (dao (unwrap! (map-get? daos dao-id) ERR_DAO_NOT_FOUND))
    (proposal-id (get proposal-counter dao))
    (next-id (+ proposal-id u1))
  )
    ;; Only DAO owner or the DAO itself can register proposals
    (asserts! (or (is-eq tx-sender (get owner dao)) (is-eq tx-sender dao-id)) ERR_UNAUTHORIZED)

    ;; Update the proposal counter
    (map-set daos dao-id (merge dao { proposal-counter: next-id }))

    ;; Store the proposal reference
    (map-set dao-proposals { dao-id: dao-id, proposal-id: proposal-id } proposal-contract)

    ;; Emit event
    (print { event: "proposal-registered", dao-id: dao-id, proposal-id: proposal-id, proposal: proposal-contract })
    (ok proposal-id)
  )
)

(define-public (update-dao-name (dao-id principal) (new-name (string-ascii 64)))
  (let (
    (dao (unwrap! (map-get? daos dao-id) ERR_DAO_NOT_FOUND))
  )
    ;; Only DAO owner or the DAO itself can update name
    (asserts! (or (is-eq tx-sender (get owner dao)) (is-eq tx-sender dao-id)) ERR_UNAUTHORIZED)
    (asserts! (> (len new-name) u0) ERR_INVALID_NAME)

    ;; Update the name
    (map-set daos dao-id (merge dao { name: new-name }))

    ;; Emit event
    (print { event: "dao-name-updated", dao-id: dao-id, new-name: new-name })
    (ok true)
  )
)

(define-public (update-dao-token (dao-id principal) (new-token principal))
  (let (
    (dao (unwrap! (map-get? daos dao-id) ERR_DAO_NOT_FOUND))
  )
    ;; Only DAO owner or the DAO itself can update token
    (asserts! (or (is-eq tx-sender (get owner dao)) (is-eq tx-sender dao-id)) ERR_UNAUTHORIZED)

    ;; Update the token
    (map-set daos dao-id (merge dao { token: new-token }))

    ;; Emit event
    (print { event: "dao-token-updated", dao-id: dao-id, new-token: new-token })
    (ok true)
  )
)

(define-public (transfer-dao-ownership (dao-id principal) (new-owner principal))
  (let (
    (dao (unwrap! (map-get? daos dao-id) ERR_DAO_NOT_FOUND))
  )
    ;; Only current DAO owner can transfer ownership
    (asserts! (is-eq tx-sender (get owner dao)) ERR_UNAUTHORIZED)

    ;; Update the owner
    (map-set daos dao-id (merge dao { owner: new-owner }))

    ;; Emit event
    (print { event: "dao-ownership-transferred", dao-id: dao-id, new-owner: new-owner })
    (ok true)
  )
)

(define-read-only (get-dao-proposal (dao-id principal) (proposal-id uint))
  (map-get? dao-proposals { dao-id: dao-id, proposal-id: proposal-id })
)

;; Implementation of the DAO trait functions for any contract using this factory
(define-read-only (impl-get-name (dao-id principal))
  (let ((dao (map-get? daos dao-id)))
    (if (is-some dao)
        (ok (get name (unwrap-panic dao)))
        (err u1)
    )
  )
)

(define-read-only (impl-get-token (dao-id principal))
  (let ((dao (map-get? daos dao-id)))
    (if (is-some dao)
        (ok (get token (unwrap-panic dao)))
        (err u1)
    )
  )
)

(define-read-only (impl-get-next-proposal-id (dao-id principal))
  (let ((dao (map-get? daos dao-id)))
    (if (is-some dao)
        (ok (get proposal-counter (unwrap-panic dao)))
        (err u1)
    )
  )
)

(define-public (impl-register-proposal (dao-id principal) (proposal-contract principal))
  (register-proposal-for-dao dao-id proposal-contract)
)

(define-read-only (impl-get-proposal (dao-id principal) (proposal-id uint))
  (let ((proposal (map-get? dao-proposals { dao-id: dao-id, proposal-id: proposal-id })))
    (if (is-some proposal)
        (ok (unwrap-panic proposal))
        (err u1)
    )
  )
)