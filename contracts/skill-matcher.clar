;; Skill Matcher Contract - 200+ lines
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u6001))
(define-constant ERR_WORKER_NOT_FOUND (err u6002))
(define-constant ERR_JOB_NOT_FOUND (err u6003))

(define-data-var next-worker-id uint u1)
(define-data-var next-job-id uint u1)
(define-data-var platform-fee uint u250)
(define-data-var total-jobs uint u0)

(define-map workers uint
  {
    provider: principal,
    name: (string-ascii 100),
    skills: (list 10 (string-ascii 50)),
    hourly-rate: uint,
    reputation-score: uint,
    completed-projects: uint,
    total-earnings: uint,
    available: bool
  }
)

(define-map job-postings uint
  {
    employer: principal,
    title: (string-ascii 200),
    description: (string-ascii 500),
    required-skills: (list 5 (string-ascii 50)),
    budget: uint,
    duration-days: uint,
    posted-at: uint,
    assigned-worker: (optional uint),
    status: uint,
    escrow-amount: uint
  }
)

(define-map worker-ratings
  { job-id: uint, rater: principal }
  {
    rating: uint,
    feedback: (string-ascii 300),
    rating-date: uint
  }
)

(define-public (register-worker (name (string-ascii 100)) (skills (list 10 (string-ascii 50))) (hourly-rate uint))
  (let ((worker-id (var-get next-worker-id)))
    (map-set workers worker-id
      {
        provider: tx-sender,
        name: name,
        skills: skills,
        hourly-rate: hourly-rate,
        reputation-score: u100,
        completed-projects: u0,
        total-earnings: u0,
        available: true
      }
    )
    (var-set next-worker-id (+ worker-id u1))
    (ok worker-id)
  )
)

(define-public (post-job (title (string-ascii 200)) (description (string-ascii 500)) (required-skills (list 5 (string-ascii 50))) (budget uint) (duration-days uint))
  (let ((job-id (var-get next-job-id)))
    (map-set job-postings job-id
      {
        employer: tx-sender,
        title: title,
        description: description,
        required-skills: required-skills,
        budget: budget,
        duration-days: duration-days,
        posted-at: block-height,
        assigned-worker: none,
        status: u1,
        escrow-amount: u0
      }
    )
    (var-set next-job-id (+ job-id u1))
    (var-set total-jobs (+ (var-get total-jobs) u1))
    (ok job-id)
  )
)

(define-public (assign-worker (job-id uint) (worker-id uint))
  (let
    (
      (job-data (unwrap! (map-get? job-postings job-id) ERR_JOB_NOT_FOUND))
      (worker-data (unwrap! (map-get? workers worker-id) ERR_WORKER_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get employer job-data)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status job-data) u1) ERR_JOB_NOT_FOUND)
    (asserts! (get available worker-data) ERR_WORKER_NOT_FOUND)
    
    (try! (stx-transfer? (get budget job-data) tx-sender (as-contract tx-sender)))
    
    (map-set job-postings job-id
      (merge job-data {
        assigned-worker: (some worker-id),
        status: u2,
        escrow-amount: (get budget job-data)
      })
    )
    (ok true)
  )
)

(define-public (complete-project (job-id uint))
  (let
    (
      (job-data (unwrap! (map-get? job-postings job-id) ERR_JOB_NOT_FOUND))
      (worker-id (unwrap! (get assigned-worker job-data) ERR_WORKER_NOT_FOUND))
      (worker-data (unwrap! (map-get? workers worker-id) ERR_WORKER_NOT_FOUND))
      (budget (get budget job-data))
      (platform-fee-amount (/ (* budget (var-get platform-fee)) u10000))
      (worker-payment (- budget platform-fee-amount))
    )
    (asserts! (is-eq tx-sender (get employer job-data)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status job-data) u2) ERR_JOB_NOT_FOUND)
    
    (try! (as-contract (stx-transfer? worker-payment tx-sender (get provider worker-data))))
    
    (map-set workers worker-id
      (merge worker-data {
        completed-projects: (+ (get completed-projects worker-data) u1),
        total-earnings: (+ (get total-earnings worker-data) worker-payment),
        reputation-score: (+ (get reputation-score worker-data) u10)
      })
    )
    
    (map-set job-postings job-id (merge job-data { status: u3 }))
    (ok worker-payment)
  )
)

(define-public (rate-worker (job-id uint) (rating uint) (feedback (string-ascii 300)))
  (let
    (
      (job-data (unwrap! (map-get? job-postings job-id) ERR_JOB_NOT_FOUND))
      (worker-id (unwrap! (get assigned-worker job-data) ERR_WORKER_NOT_FOUND))
      (worker-data (unwrap! (map-get? workers worker-id) ERR_WORKER_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get employer job-data)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status job-data) u3) ERR_JOB_NOT_FOUND)
    (asserts! (and (>= rating u1) (<= rating u5)) ERR_NOT_AUTHORIZED)
    
    (map-set worker-ratings { job-id: job-id, rater: tx-sender }
      {
        rating: rating,
        feedback: feedback,
        rating-date: block-height
      }
    )
    
    (map-set workers worker-id
      (merge worker-data {
        reputation-score: (+ (get reputation-score worker-data) (* rating u10))
      })
    )
    (ok true)
  )
)

(define-read-only (get-worker (worker-id uint))
  (map-get? workers worker-id)
)

(define-read-only (get-job (job-id uint))
  (map-get? job-postings job-id)
)

(define-read-only (get-worker-rating (job-id uint) (rater principal))
  (map-get? worker-ratings { job-id: job-id, rater: rater })
)

(define-read-only (get-platform-stats)
  {
    total-jobs: (var-get total-jobs),
    platform-fee: (var-get platform-fee)
  }
)
