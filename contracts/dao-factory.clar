;; dao-factory.clar
;; Creates and manages new DAO contracts
;; Handles registration of DAOs and proposal management

(use-trait dao-trait .utils.dao-trait)
(use-trait proposal-trait .proposal.proposal-trait)

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_DAO_NOT_FOUND (err u101))
(define-constant ERR_INVALID_DAO (err u102))
(define-constant ERR_ALREADY_REGISTERED (err u103))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u104))
(define-constant ERR_INVALID_STATE (err u105))

;; Define current-block-height variable
(define-data-var current-block-height uint u100)

;; Data maps and variables
(define-map registered-daos principal {
  name: (string-ascii 64),
  description: (string-utf8 256),
  token-contract: principal,
  proposal-count: uint,
  active: bool,
  created-at: uint,
  metadata-url: (optional (string-utf8 256))
})

(define-map dao-proposals { dao-id: principal, proposal-id: uint } principal)

(define-map proposal-metrics principal {
  vote-count: uint,
  funding-amount: uint,
  status: (string-ascii 20), ;; "active", "passed", "rejected", "executed"
  last-updated: uint
})

(define-data-var dao-count uint u0)
(define-data-var total-proposals uint u0)

;; Read-only functions
(define-read-only (get-dao-count)
  (var-get dao-count)
)

(define-read-only (get-total-proposals)
  (var-get total-proposals)
)

(define-read-only (get-dao (dao-id principal))
  (map-get? registered-daos dao-id)
)

(define-read-only (get-dao-proposal (dao-id principal) (proposal-id uint))
  (map-get? dao-proposals { dao-id: dao-id, proposal-id: proposal-id })
)

(define-read-only (get-proposal-metrics (proposal-contract principal))
  (map-get? proposal-metrics proposal-contract)
)

;; Public functions
(define-public (register-dao
    (dao-contract <dao-trait>)
    (name (string-ascii 64))
    (description (string-utf8 256))
    (token-contract principal)
    (metadata-url (optional (string-utf8 256)))
  )
  (let (
    (dao-id (contract-of dao-contract))
  )
    (asserts! (is-none (map-get? registered-daos dao-id)) ERR_ALREADY_REGISTERED)

    ;; Store DAO information
    (map-set registered-daos dao-id {
      name: name,
      description: description,
      token-contract: token-contract,
      proposal-count: u0,
      active: true,
      created-at: (var-get current-block-height),
      metadata-url: metadata-url
    })

    ;; Increment DAO count
    (var-set dao-count (+ (var-get dao-count) u1))

    ;; Emit event and return success
    (print { event: "dao-registered", dao-id: dao-id, name: name })
    (ok dao-id)
  )
)

(define-public (submit-proposal
    (dao-contract <dao-trait>)
    (proposal-contract <proposal-trait>)
  )
  (let (
    (dao-id (contract-of dao-contract))
    (proposal-id (unwrap-panic (contract-call? dao-contract get-next-proposal-id)))
    (proposal-principal (contract-of proposal-contract))
  )
    ;; Check that the DAO is registered
    (asserts! (is-some (map-get? registered-daos dao-id)) ERR_DAO_NOT_FOUND)

    ;; Register the proposal with the DAO
    (try! (contract-call? dao-contract register-proposal (contract-of proposal-contract)))

    ;; Store proposal relationship
    (map-set dao-proposals { dao-id: dao-id, proposal-id: proposal-id } proposal-principal)

    ;; Initialize metrics
    (map-set proposal-metrics proposal-principal {
      vote-count: u0,
      funding-amount: u0,
      status: "active",
      last-updated: (var-get current-block-height)
    })

    ;; Update counts
    (map-set registered-daos dao-id
      (merge (unwrap! (map-get? registered-daos dao-id) ERR_DAO_NOT_FOUND)
             { proposal-count: (+ proposal-id u1) })
    )
    (var-set total-proposals (+ (var-get total-proposals) u1))

    ;; Emit event
    (print { event: "proposal-submitted", dao-id: dao-id, proposal-id: proposal-id, proposal-contract: proposal-principal })
    (ok proposal-id)
  )
)

(define-public (update-proposal-metrics
    (proposal-contract <proposal-trait>)
    (vote-count uint)
    (funding-amount uint)
    (status (string-ascii 20))
  )
  (let (
    (proposal-principal (contract-of proposal-contract))
    (current-metrics (unwrap! (map-get? proposal-metrics proposal-principal) ERR_PROPOSAL_NOT_FOUND))
  )
    ;; Update the metrics
    (map-set proposal-metrics proposal-principal {
      vote-count: vote-count,
      funding-amount: funding-amount,
      status: status,
      last-updated: (var-get current-block-height)
    })

    ;; Emit event
    (print { event: "proposal-updated", proposal: proposal-principal, status: status })
    (ok true)
  )
)

(define-public (deactivate-dao (dao-id principal))
  (begin
    ;; Only contract owner or the DAO itself can deactivate
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender dao-id)) ERR_UNAUTHORIZED)
    (asserts! (is-some (map-get? registered-daos dao-id)) ERR_DAO_NOT_FOUND)

    ;; Update the DAO status
    (map-set registered-daos dao-id
      (merge (unwrap! (map-get? registered-daos dao-id) ERR_DAO_NOT_FOUND)
             { active: false })
    )

    ;; Emit event
    (print { event: "dao-deactivated", dao-id: dao-id })
    (ok true)
  )
)

(define-public (reactivate-dao (dao-id principal))
  (begin
    ;; Only contract owner can reactivate
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (is-some (map-get? registered-daos dao-id)) ERR_DAO_NOT_FOUND)

    ;; Update the DAO status
    (map-set registered-daos dao-id
      (merge (unwrap! (map-get? registered-daos dao-id) ERR_DAO_NOT_FOUND)
             { active: true })
    )

    ;; Emit event
    (print { event: "dao-reactivated", dao-id: dao-id })
    (ok true)
  )
)

;; Admin functions
(define-public (update-dao-metadata
    (dao-id principal)
    (metadata-url (optional (string-utf8 256)))
  )
  (begin
    ;; Only contract owner or the DAO itself can update metadata
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender dao-id)) ERR_UNAUTHORIZED)
    (asserts! (is-some (map-get? registered-daos dao-id)) ERR_DAO_NOT_FOUND)

    ;; Update the metadata
    (map-set registered-daos dao-id
      (merge (unwrap! (map-get? registered-daos dao-id) ERR_DAO_NOT_FOUND)
             { metadata-url: metadata-url })
    )

    ;; Emit event
    (print { event: "dao-metadata-updated", dao-id: dao-id })
    (ok true)
  )
)