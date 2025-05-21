;; proposal.clar
;; Handles proposal data structure and lifecycle management

;; Define traits
(define-trait proposal-trait
  (
    (get-title () (response (string-ascii 128) uint))
    (get-description () (response (string-utf8 2048) uint))
    (get-status () (response (string-ascii 20) uint))
    (get-proposer () (response principal uint))
    (get-vote-counts () (response {yes: uint, no: uint, abstain: uint} uint))
    (get-funding-goal () (response uint uint))
    (get-current-funding () (response uint uint))
    (vote (uint) (response bool uint))
    (execute () (response bool uint))
  )
)

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_STATUS (err u101))
(define-constant ERR_ALREADY_VOTED (err u102))
(define-constant ERR_VOTING_CLOSED (err u103))
(define-constant ERR_INSUFFICIENT_VOTES (err u104))
(define-constant ERR_ALREADY_EXECUTED (err u105))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u106))

;; Define current-block-height variable
(define-data-var current-block-height uint u100)

;; Proposal status types
(define-constant STATUS_DRAFT "draft")
(define-constant STATUS_ACTIVE "active")
(define-constant STATUS_PASSED "passed")
(define-constant STATUS_REJECTED "rejected")
(define-constant STATUS_EXECUTED "executed")
(define-constant STATUS_CANCELED "canceled")

;; Vote types
(define-constant VOTE_YES u1)
(define-constant VOTE_NO u2)
(define-constant VOTE_ABSTAIN u3)

;; Data structures
(define-map proposals principal {
  title: (string-ascii 128),
  description: (string-utf8 2048),
  dao-id: principal,
  proposer: principal,
  status: (string-ascii 20),
  created-at: uint,
  start-block: uint,
  end-block: uint,
  execute-data: (optional (buff 1024)),
  metadata-url: (optional (string-utf8 256)),
  funding-goal: uint,
  min-approval-percentage: uint
})

(define-map proposal-votes {proposal-id: principal, voter: principal} {
  vote-type: uint,
  voting-power: uint,
  block-height: uint
})

(define-map proposal-vote-counts principal {
  yes: uint,
  no: uint,
  abstain: uint,
  total-voted: uint
})

(define-map proposal-funding principal {
  current-amount: uint,
  backers: uint
})

;; Read-only functions
(define-read-only (get-proposal (proposal-id principal))
  (map-get? proposals proposal-id)
)

(define-read-only (get-vote (proposal-id principal) (voter principal))
  (map-get? proposal-votes {proposal-id: proposal-id, voter: voter})
)

(define-read-only (get-vote-counts (proposal-id principal))
  (default-to
    {yes: u0, no: u0, abstain: u0, total-voted: u0}
    (map-get? proposal-vote-counts proposal-id)
  )
)

(define-read-only (get-funding-info (proposal-id principal))
  (default-to
    {current-amount: u0, backers: u0}
    (map-get? proposal-funding proposal-id)
  )
)

(define-read-only (is-open (proposal-id principal))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) false))
    (current-block (var-get current-block-height))
  )
    (and
      (is-eq (get status proposal) STATUS_ACTIVE)
      (>= current-block (get start-block proposal))
      (<= current-block (get end-block proposal))
    )
  )
)

;; Public functions
(define-public (create-proposal
    (title (string-ascii 128))
    (description (string-utf8 2048))
    (dao-id principal)
    (start-block uint)
    (end-block uint)
    (execute-data (optional (buff 1024)))
    (metadata-url (optional (string-utf8 256)))
    (funding-goal uint)
    (min-approval-percentage uint)
  )
  (let (
    (proposal-id (as-contract tx-sender))
  )
    ;; Validate inputs
    (asserts! (> (len title) u0) (err u107))
    (asserts! (> (len description) u0) (err u108))
    (asserts! (>= end-block start-block) (err u109))
    (asserts! (<= min-approval-percentage u100) (err u110))

    ;; Create the proposal
    (map-set proposals proposal-id {
      title: title,
      description: description,
      dao-id: dao-id,
      proposer: tx-sender,
      status: STATUS_DRAFT,
      created-at: (var-get current-block-height),
      start-block: start-block,
      end-block: end-block,
      execute-data: execute-data,
      metadata-url: metadata-url,
      funding-goal: funding-goal,
      min-approval-percentage: min-approval-percentage
    })

    ;; Initialize vote counts
    (map-set proposal-vote-counts proposal-id {
      yes: u0,
      no: u0,
      abstain: u0,
      total-voted: u0
    })

    ;; Initialize funding
    (map-set proposal-funding proposal-id {
      current-amount: u0,
      backers: u0
    })

    ;; Emit event
    (print {event: "proposal-created", proposal-id: proposal-id, dao: dao-id, proposer: tx-sender})
    (ok proposal-id)
  )
)

(define-public (activate-proposal (proposal-id principal))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
  )
    ;; Only proposer or DAO can activate
    (asserts! (or (is-eq tx-sender (get proposer proposal))
                (is-eq tx-sender (get dao-id proposal)))
              ERR_UNAUTHORIZED)

    ;; Validate status
    (asserts! (is-eq (get status proposal) STATUS_DRAFT) ERR_INVALID_STATUS)

    ;; Update status
    (map-set proposals proposal-id
      (merge proposal {status: STATUS_ACTIVE})
    )

    ;; Emit event
    (print {event: "proposal-activated", proposal-id: proposal-id})
    (ok true)
  )
)

(define-public (cast-vote
    (proposal-id principal)
    (vote-type uint)
    (voting-power uint)
  )
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
    (current-votes (default-to
      {yes: u0, no: u0, abstain: u0, total-voted: u0}
      (map-get? proposal-vote-counts proposal-id)))
  )
    ;; Check if voting is open
    (asserts! (is-open proposal-id) ERR_VOTING_CLOSED)

    ;; Check if already voted
    (asserts! (is-none (get-vote proposal-id tx-sender)) ERR_ALREADY_VOTED)

    ;; Record the vote
    (map-set proposal-votes {proposal-id: proposal-id, voter: tx-sender} {
      vote-type: vote-type,
      voting-power: voting-power,
      block-height: (var-get current-block-height)
    })

    ;; Update vote counts based on vote type
    (map-set proposal-vote-counts proposal-id
      (if (is-eq vote-type VOTE_YES)
        (merge current-votes {
          yes: (+ (get yes current-votes) voting-power),
          total-voted: (+ (get total-voted current-votes) voting-power)
        })
        (if (is-eq vote-type VOTE_NO)
          (merge current-votes {
            no: (+ (get no current-votes) voting-power),
            total-voted: (+ (get total-voted current-votes) voting-power)
          })
          (if (is-eq vote-type VOTE_ABSTAIN)
            (merge current-votes {
              abstain: (+ (get abstain current-votes) voting-power),
              total-voted: (+ (get total-voted current-votes) voting-power)
            })
            current-votes
          )
        )
      )
    )

    ;; Emit event
    (print {event: "vote-cast", proposal-id: proposal-id, voter: tx-sender, vote-type: vote-type, voting-power: voting-power})
    (ok true)
  )
)

(define-public (finalize-proposal (proposal-id principal))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
    (vote-counts (get-vote-counts proposal-id))
    (yes-votes (get yes vote-counts))
    (no-votes (get no vote-counts))
    (total-votes (+ yes-votes no-votes))
    (approval-percentage (if (> total-votes u0)
                           (/ (* yes-votes u100) total-votes)
                           u0))
  )
    ;; Check if proposal is active and voting has ended
    (asserts! (is-eq (get status proposal) STATUS_ACTIVE) ERR_INVALID_STATUS)
    (asserts! (>= (var-get current-block-height) (get end-block proposal)) ERR_VOTING_CLOSED)

    ;; Determine new status based on votes
    (if (>= approval-percentage (get min-approval-percentage proposal))
      (begin
        (map-set proposals proposal-id (merge proposal {status: STATUS_PASSED}))
        (print {event: "proposal-passed", proposal-id: proposal-id, approval: approval-percentage})
      )
      (begin
        (map-set proposals proposal-id (merge proposal {status: STATUS_REJECTED}))
        (print {event: "proposal-rejected", proposal-id: proposal-id, approval: approval-percentage})
      )
    )

    (ok true)
  )
)

(define-public (execute-proposal (proposal-id principal))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
  )
    ;; Check if proposal is in passed status
    (asserts! (is-eq (get status proposal) STATUS_PASSED) ERR_INVALID_STATUS)

    ;; Mark as executed
    (map-set proposals proposal-id (merge proposal {status: STATUS_EXECUTED}))

    ;; Emit event
    (print {event: "proposal-executed", proposal-id: proposal-id, executor: tx-sender})
    (ok true)
  )
)

(define-public (cancel-proposal (proposal-id principal))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
  )
    ;; Only proposer or DAO can cancel
    (asserts! (or (is-eq tx-sender (get proposer proposal))
                (is-eq tx-sender (get dao-id proposal)))
              ERR_UNAUTHORIZED)

    ;; Cannot cancel if already executed or finalized
    (asserts! (not (or (is-eq (get status proposal) STATUS_EXECUTED)
                      (is-eq (get status proposal) STATUS_PASSED)
                      (is-eq (get status proposal) STATUS_REJECTED)))
              ERR_INVALID_STATUS)

    ;; Update status
    (map-set proposals proposal-id (merge proposal {status: STATUS_CANCELED}))

    ;; Emit event
    (print {event: "proposal-canceled", proposal-id: proposal-id, canceler: tx-sender})
    (ok true)
  )
)

;; Implementation of trait functions
(define-read-only (get-title (proposal-id principal))
  (match (map-get? proposals proposal-id)
    proposal (ok (get title proposal))
    (err u1)
  )
)

(define-read-only (get-description (proposal-id principal))
  (match (map-get? proposals proposal-id)
    proposal (ok (get description proposal))
    (err u1)
  )
)

(define-read-only (get-status (proposal-id principal))
  (match (map-get? proposals proposal-id)
    proposal (ok (get status proposal))
    (err u1)
  )
)

(define-read-only (get-proposer (proposal-id principal))
  (match (map-get? proposals proposal-id)
    proposal (ok (get proposer proposal))
    (err u1)
  )
)

(define-read-only (get-vote-counts-trait (proposal-id principal))
  (let ((counts (get-vote-counts proposal-id)))
    (ok {
      yes: (get yes counts),
      no: (get no counts),
      abstain: (get abstain counts)
    })
  )
)

(define-read-only (get-funding-goal (proposal-id principal))
  (match (map-get? proposals proposal-id)
    proposal (ok (get funding-goal proposal))
    (err u1)
  )
)

(define-read-only (get-current-funding (proposal-id principal))
  (ok (get current-amount (get-funding-info proposal-id)))
)

(define-public (vote (proposal-id principal) (vote-type uint))
  (cast-vote proposal-id vote-type u1)
)

(define-public (execute (proposal-id principal))
  (execute-proposal proposal-id)
)