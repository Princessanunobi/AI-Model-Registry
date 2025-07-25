;; Decentralized AI Model Registry and Ranking Platform
;; A comprehensive smart contract enabling AI model registration, community-driven evaluation,
;; reputation-based scoring, and decentralized governance of AI model quality rankings.
;; Features include staking mechanisms, weighted voting, category-based organization,
;; and anti-spam protection through economic incentives.

;; CONSTANTS AND CONFIGURATION

;; Contract governance
(define-constant contract-administrator tx-sender)

;; Error codes for comprehensive error handling
(define-constant ERR-UNAUTHORIZED-ACCESS (err u100))
(define-constant ERR-MODEL-RECORD-NOT-FOUND (err u101))
(define-constant ERR-DUPLICATE-VOTE-ATTEMPT (err u102))
(define-constant ERR-INVALID-RATING-SCORE (err u103))
(define-constant ERR-MODEL-ALREADY-REGISTERED (err u104))
(define-constant ERR-INSUFFICIENT-STAKE-BALANCE (err u105))
(define-constant ERR-INVALID-MODEL-CATEGORY (err u106))
(define-constant ERR-MODEL-DEACTIVATED-STATUS (err u107))
(define-constant ERR-WITHDRAWAL-TIME-RESTRICTION (err u108))
(define-constant ERR-INVALID-STRING-LENGTH (err u109))
(define-constant ERR-INVALID-COMMENT-LENGTH (err u110))

;; Economic parameters
(define-constant minimum-registration-stake u1000000) ;; 1 STX in microSTX
(define-constant stake-lockup-period u100) ;; blocks before withdrawal allowed
(define-constant reputation-weight-multiplier u100)
(define-constant maximum-rating-score u10)
(define-constant minimum-rating-score u1)

;; System configuration
(define-constant maximum-models-per-category u10)
(define-constant model-name-max-length u100)
(define-constant model-description-max-length u500)
(define-constant vote-comment-max-length u200)
(define-constant category-name-max-length u30)
(define-constant ipfs-hash-length u64)

;; Supported AI model categories - Fixed to use consistent string-ascii 30
(define-constant supported-model-categories 
  (list 
    "natural-language-processing"
    "computer-vision"
    "recommendation-systems"
    "reinforcement-learning"
    "generative-models"
    "speech-recognition"
    "time-series-analysis"
    "other-category"))

;; STATE VARIABLES

(define-data-var next-available-model-identifier uint u1)
(define-data-var total-registered-models uint u0)
(define-data-var platform-initialization-status bool false)

;; DATA STRUCTURES AND MAPS

;; Comprehensive AI model registry
(define-map registered-ai-models
  { model-identifier: uint }
  {
    model-display-name: (string-ascii 100),
    detailed-description: (string-utf8 500),
    model-creator-address: principal,
    assigned-category: (string-ascii 30),
    content-hash-reference: (string-ascii 64),
    accumulated-vote-count: uint,
    cumulative-score-total: uint,
    calculated-average-rating: uint,
    required-stake-amount: uint,
    registration-block-height: uint,
    current-active-status: bool,
    last-updated-block: uint
  }
)

;; Community voting and evaluation system
(define-map community-evaluation-records
  { evaluator-address: principal, target-model-identifier: uint }
  {
    assigned-rating-score: uint,
    evaluation-timestamp: uint,
    optional-feedback-comment: (optional (string-utf8 200)),
    evaluator-reputation-at-vote: uint
  }
)

;; User economic stake tracking
(define-map participant-stake-balances
  { participant-address: principal }
  { 
    total-staked-amount: uint,
    active-model-stakes: (list 50 uint)
  }
)

;; Dynamic category-based model rankings
(define-map category-leaderboards
  { category-identifier: (string-ascii 30) }
  { 
    ranked-model-identifiers: (list 10 uint),
    leaderboard-last-update: uint,
    category-total-models: uint
  }
)

;; Participant reputation and activity tracking
(define-map participant-reputation-profiles
  { participant-address: principal }
  {
    accumulated-reputation-points: uint,
    total-evaluations-submitted: uint,
    total-models-contributed: uint,
    quality-score-average: uint,
    account-creation-block: uint
  }
)

;; Model performance analytics
(define-map model-analytics-data
  { model-identifier: uint }
  {
    weekly-vote-count: uint,
    monthly-vote-count: uint,
    trending-score: uint,
    quality-consistency-rating: uint
  }
)
;; READ-ONLY QUERY FUNCTIONS

;; Retrieve complete model information
(define-read-only (fetch-model-details (target-model-identifier uint))
  (map-get? registered-ai-models { model-identifier: target-model-identifier })
)

;; Get user's evaluation for specific model
(define-read-only (fetch-user-evaluation (evaluator-address principal) (target-model-identifier uint))
  (map-get? community-evaluation-records { evaluator-address: evaluator-address, target-model-identifier: target-model-identifier })
)

;; Retrieve participant reputation profile
(define-read-only (fetch-reputation-profile (participant-address principal))
  (default-to 
    { 
      accumulated-reputation-points: u0, 
      total-evaluations-submitted: u0, 
      total-models-contributed: u0,
      quality-score-average: u0,
      account-creation-block: u0
    }
    (map-get? participant-reputation-profiles { participant-address: participant-address })
  )
)

;; Get category leaderboard rankings
(define-read-only (fetch-category-leaderboard (category-identifier (string-ascii 30)))
  (map-get? category-leaderboards { category-identifier: category-identifier })
)

;; Query total platform statistics
(define-read-only (fetch-platform-statistics)
  {
    total-models: (var-get total-registered-models),
    next-model-id: (var-get next-available-model-identifier),
    platform-initialized: (var-get platform-initialization-status)
  }
)

;; Validate category membership
(define-read-only (validate-model-category (category-identifier (string-ascii 30)))
  (or 
    (is-eq category-identifier "natural-language-processing")
    (is-eq category-identifier "computer-vision")
    (is-eq category-identifier "recommendation-systems")
    (is-eq category-identifier "reinforcement-learning")
    (is-eq category-identifier "generative-models")
    (is-eq category-identifier "speech-recognition")
    (is-eq category-identifier "time-series-analysis")
    (is-eq category-identifier "other-category")
  )
)

;; Calculate reputation-weighted evaluation score
(define-read-only (compute-weighted-evaluation-score 
  (base-rating-score uint) 
  (evaluator-reputation-points uint))
  (let ((reputation-weight-factor (+ u1 (/ evaluator-reputation-points reputation-weight-multiplier))))
    (* base-rating-score reputation-weight-factor)
  )
)

;; Retrieve participant's total stake balance
(define-read-only (fetch-participant-stake-balance (participant-address principal))
  (default-to u0 
    (get total-staked-amount 
      (map-get? participant-stake-balances { participant-address: participant-address })))
)

;; Check model active status
(define-read-only (verify-model-active-status (target-model-identifier uint))
  (match (fetch-model-details target-model-identifier)
    model-record (get current-active-status model-record)
    false
  )
)

;; Get models by category with pagination
(define-read-only (fetch-models-in-category (category-identifier (string-ascii 30)))
  (match (fetch-category-leaderboard category-identifier)
    leaderboard-data (get ranked-model-identifiers leaderboard-data)
    (list)
  )
)

;; INPUT VALIDATION HELPER FUNCTIONS

;; Validate comment length if provided
(define-private (validate-comment-length (comment-option (optional (string-utf8 200))))
  (match comment-option
    some-comment (and (> (len some-comment) u0) (<= (len some-comment) vote-comment-max-length))
    true  ;; None is always valid
  )
)

;; Validate model identifier bounds
(define-private (validate-model-identifier (model-id uint))
  (and (> model-id u0) (< model-id (var-get next-available-model-identifier)))
)

;; CORE FUNCTIONALITY FUNCTIONS

;; Register new AI model with comprehensive validation
(define-public (register-ai-model 
  (model-display-name (string-ascii 100))
  (detailed-description (string-utf8 500))
  (assigned-category (string-ascii 30))
  (content-hash-reference (string-ascii 64))
)
  (let (
    (new-model-identifier (var-get next-available-model-identifier))
    (current-block-height stacks-block-height)
    (registrant-address tx-sender)
  )
    (asserts! (validate-model-category assigned-category) ERR-INVALID-MODEL-CATEGORY)
    (asserts! (> (len model-display-name) u0) ERR-INVALID-STRING-LENGTH)
    (asserts! (> (len detailed-description) u0) ERR-INVALID-STRING-LENGTH)
    (asserts! (is-eq (len content-hash-reference) ipfs-hash-length) ERR-INVALID-STRING-LENGTH)
    
    ;; Economic requirements validation
    (asserts! (>= (stx-get-balance registrant-address) minimum-registration-stake) ERR-INSUFFICIENT-STAKE-BALANCE)
    
    ;; Execute stake transfer to contract
    (try! (stx-transfer? minimum-registration-stake registrant-address (as-contract tx-sender)))
    
    ;; Register model in system
    (map-set registered-ai-models
      { model-identifier: new-model-identifier }
      {
        model-display-name: model-display-name,
        detailed-description: detailed-description,
        model-creator-address: registrant-address,
        assigned-category: assigned-category,
        content-hash-reference: content-hash-reference,
        accumulated-vote-count: u0,
        cumulative-score-total: u0,
        calculated-average-rating: u0,
        required-stake-amount: minimum-registration-stake,
        registration-block-height: current-block-height,
        current-active-status: true,
        last-updated-block: current-block-height
      }
    )
    
    ;; Update participant stake tracking
    (update-participant-stake-record registrant-address minimum-registration-stake new-model-identifier)
    
    ;; Update reputation profile
    (increment-participant-reputation registrant-address "model-registration")
    
    ;; Update system counters
    (var-set next-available-model-identifier (+ new-model-identifier u1))
    (var-set total-registered-models (+ (var-get total-registered-models) u1))
    
    ;; Update category leaderboard
    (refresh-category-leaderboard assigned-category)
    
    (ok new-model-identifier)
  )
)

;; Submit community evaluation with anti-spam protection - FIXED WARNINGS
(define-public (submit-model-evaluation 
  (target-model-identifier uint) 
  (assigned-rating-score uint) 
  (optional-feedback-comment (optional (string-utf8 200))))
  (let (
    (target-model-record (unwrap! (fetch-model-details target-model-identifier) ERR-MODEL-RECORD-NOT-FOUND))
    (current-block-height stacks-block-height)
    (evaluator-address tx-sender)
    (evaluator-reputation-profile (fetch-reputation-profile evaluator-address))
    (weighted-evaluation-score (compute-weighted-evaluation-score 
                                assigned-rating-score 
                                (get accumulated-reputation-points evaluator-reputation-profile)))
    ;; VALIDATION: Check inputs before using them
    (validated-comment (begin 
                        (asserts! (validate-comment-length optional-feedback-comment) ERR-INVALID-COMMENT-LENGTH)
                        optional-feedback-comment))
  )
    ;; Comprehensive validation checks
    (asserts! (and (>= assigned-rating-score minimum-rating-score) 
                   (<= assigned-rating-score maximum-rating-score)) ERR-INVALID-RATING-SCORE)
    (asserts! (is-none (fetch-user-evaluation evaluator-address target-model-identifier)) ERR-DUPLICATE-VOTE-ATTEMPT)
    (asserts! (get current-active-status target-model-record) ERR-MODEL-DEACTIVATED-STATUS)
    
    ;; Record evaluation in system - Use validated comment
    (map-set community-evaluation-records
      { evaluator-address: evaluator-address, target-model-identifier: target-model-identifier }
      {
        assigned-rating-score: assigned-rating-score,
        evaluation-timestamp: current-block-height,
        optional-feedback-comment: validated-comment,
        evaluator-reputation-at-vote: (get accumulated-reputation-points evaluator-reputation-profile)
      }
    )
    
    ;; Update model statistics with weighted scoring
    (let (
      (updated-vote-count (+ (get accumulated-vote-count target-model-record) u1))
      (updated-score-total (+ (get cumulative-score-total target-model-record) weighted-evaluation-score))
      (updated-average-rating (/ updated-score-total updated-vote-count))
    )
      (map-set registered-ai-models
        { model-identifier: target-model-identifier }
        (merge target-model-record {
          accumulated-vote-count: updated-vote-count,
          cumulative-score-total: updated-score-total,
          calculated-average-rating: updated-average-rating,
          last-updated-block: current-block-height
        })
      )
    )
    
    ;; Update evaluator reputation
    (increment-participant-reputation evaluator-address "eval-submission")
    
    ;; Refresh category rankings
    (refresh-category-leaderboard (get assigned-category target-model-record))
    
    (ok true)
  )
)

;; Stake withdrawal with time-lock protection
(define-public (withdraw-model-stake (target-model-identifier uint))
  (let (
    (target-model-record (unwrap! (fetch-model-details target-model-identifier) ERR-MODEL-RECORD-NOT-FOUND))
    (current-block-height stacks-block-height)
  )
    ;; Authorization and timing validation
    (asserts! (is-eq tx-sender (get model-creator-address target-model-record)) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (> current-block-height 
                 (+ (get registration-block-height target-model-record) stake-lockup-period)) 
              ERR-WITHDRAWAL-TIME-RESTRICTION)
    
    ;; Execute stake return
    (try! (as-contract (stx-transfer? 
                        (get required-stake-amount target-model-record) 
                        tx-sender 
                        (get model-creator-address target-model-record))))
    
    ;; Update participant stake tracking
    (reduce-participant-stake-record tx-sender (get required-stake-amount target-model-record))
    
    ;; Deactivate model
    (map-set registered-ai-models
      { model-identifier: target-model-identifier }
      (merge target-model-record { 
        current-active-status: false,
        last-updated-block: current-block-height
      })
    )
    
    (ok true)
  )
)
;; ADMINISTRATIVE AND UTILITY FUNCTIONS

;; Administrative model deactivation - FIXED WARNINGS
(define-public (deactivate-model-admin (target-model-identifier uint))
  (let (
    ;; VALIDATION: Check model identifier before using it
    (validated-model-id (begin 
                          (asserts! (validate-model-identifier target-model-identifier) ERR-MODEL-RECORD-NOT-FOUND)
                          target-model-identifier))
    (target-model-record (unwrap! (fetch-model-details validated-model-id) ERR-MODEL-RECORD-NOT-FOUND))
    (current-block-height stacks-block-height)
  )
    ;; Administrative authorization
    (asserts! (is-eq tx-sender contract-administrator) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Execute deactivation using validated model ID
    (map-set registered-ai-models
      { model-identifier: validated-model-id }
      (merge target-model-record { 
        current-active-status: false,
        last-updated-block: current-block-height
      })
    )
    
    (ok true)
  )
)

;; Platform initialization with category setup
(define-public (initialize-platform)
  (begin
    ;; Initialize all supported categories
    (map-set category-leaderboards 
      { category-identifier: "natural-language-processing" } 
      { ranked-model-identifiers: (list), leaderboard-last-update: stacks-block-height, category-total-models: u0 })
    (map-set category-leaderboards 
      { category-identifier: "computer-vision" } 
      { ranked-model-identifiers: (list), leaderboard-last-update: stacks-block-height, category-total-models: u0 })
    (map-set category-leaderboards 
      { category-identifier: "recommendation-systems" } 
      { ranked-model-identifiers: (list), leaderboard-last-update: stacks-block-height, category-total-models: u0 })
    (map-set category-leaderboards 
      { category-identifier: "reinforcement-learning" } 
      { ranked-model-identifiers: (list), leaderboard-last-update: stacks-block-height, category-total-models: u0 })
    (map-set category-leaderboards 
      { category-identifier: "generative-models" } 
      { ranked-model-identifiers: (list), leaderboard-last-update: stacks-block-height, category-total-models: u0 })
    (map-set category-leaderboards 
      { category-identifier: "speech-recognition" } 
      { ranked-model-identifiers: (list), leaderboard-last-update: stacks-block-height, category-total-models: u0 })
    (map-set category-leaderboards 
      { category-identifier: "time-series-analysis" } 
      { ranked-model-identifiers: (list), leaderboard-last-update: stacks-block-height, category-total-models: u0 })
    (map-set category-leaderboards 
      { category-identifier: "other-category" } 
      { ranked-model-identifiers: (list), leaderboard-last-update: stacks-block-height, category-total-models: u0 })
    
    ;; Mark platform as initialized
    (var-set platform-initialization-status true)
    (ok true)
  )
)

;; INTERNAL HELPER FUNCTIONS

;; Update participant stake records - Fixed to handle optional values properly
(define-private (update-participant-stake-record 
  (participant-address principal) 
  (additional-stake-amount uint) 
  (model-identifier uint))
  (let (
    (current-stake-record (map-get? participant-stake-balances { participant-address: participant-address }))
  )
    (match current-stake-record
      some-record 
        (let (
          (existing-total-stake (get total-staked-amount some-record))
          (existing-model-stakes (get active-model-stakes some-record))
        )
          (map-set participant-stake-balances
            { participant-address: participant-address }
            {
              total-staked-amount: (+ existing-total-stake additional-stake-amount),
              active-model-stakes: (unwrap-panic (as-max-len? (append existing-model-stakes model-identifier) u50))
            }
          )
        )
      ;; Create new record if none exists
      (map-set participant-stake-balances
        { participant-address: participant-address }
        {
          total-staked-amount: additional-stake-amount,
          active-model-stakes: (list model-identifier)
        }
      )
    )
  )
)

;; Reduce participant stake balance - Fixed to handle unwrap properly
(define-private (reduce-participant-stake-record 
  (participant-address principal) 
  (withdrawn-stake-amount uint))
  (let (
    (current-stake-record (map-get? participant-stake-balances { participant-address: participant-address }))
  )
    (match current-stake-record
      some-record 
        (let (
          (current-total-stake (get total-staked-amount some-record))
        )
          (map-set participant-stake-balances
            { participant-address: participant-address }
            (merge some-record {
              total-staked-amount: (- current-total-stake withdrawn-stake-amount)
            })
          )
        )
      ;; Do nothing if no record exists
      false
    )
  )
)

;; Increment participant reputation based on activity type - Fixed to return proper response
(define-private (increment-participant-reputation 
  (participant-address principal) 
  (activity-type (string-ascii 20)))
  (let (
    (current-reputation-profile (fetch-reputation-profile participant-address))
    (current-block-height stacks-block-height)
  )
    (if (is-eq activity-type "model-registration")
      (map-set participant-reputation-profiles
        { participant-address: participant-address }
        {
          accumulated-reputation-points: (+ (get accumulated-reputation-points current-reputation-profile) u5),
          total-evaluations-submitted: (get total-evaluations-submitted current-reputation-profile),
          total-models-contributed: (+ (get total-models-contributed current-reputation-profile) u1),
          quality-score-average: (get quality-score-average current-reputation-profile),
          account-creation-block: (if (is-eq (get account-creation-block current-reputation-profile) u0) 
                                    current-block-height 
                                    (get account-creation-block current-reputation-profile))
        })
      (map-set participant-reputation-profiles
        { participant-address: participant-address }
        {
          accumulated-reputation-points: (+ (get accumulated-reputation-points current-reputation-profile) u1),
          total-evaluations-submitted: (+ (get total-evaluations-submitted current-reputation-profile) u1),
          total-models-contributed: (get total-models-contributed current-reputation-profile),
          quality-score-average: (get quality-score-average current-reputation-profile),
          account-creation-block: (if (is-eq (get account-creation-block current-reputation-profile) u0) 
                                    current-block-height 
                                    (get account-creation-block current-reputation-profile))
        })
    )
  )
)

;; Refresh category leaderboard rankings - Fixed to return proper response
(define-private (refresh-category-leaderboard (category-identifier (string-ascii 30)))
  (let (
    (current-leaderboard-data (fetch-category-leaderboard category-identifier))
    (current-block-height stacks-block-height)
  )
    ;; Simplified leaderboard update - in production, implement sophisticated ranking algorithm
    (match current-leaderboard-data
      some-data 
        (map-set category-leaderboards
          { category-identifier: category-identifier }
          {
            ranked-model-identifiers: (get ranked-model-identifiers some-data),
            leaderboard-last-update: current-block-height,
            category-total-models: (+ (get category-total-models some-data) u1)
          })
      ;; Create new leaderboard if none exists
      (map-set category-leaderboards
        { category-identifier: category-identifier }
        {
          ranked-model-identifiers: (list),
          leaderboard-last-update: current-block-height,
          category-total-models: u1
        })
    )
  )
)