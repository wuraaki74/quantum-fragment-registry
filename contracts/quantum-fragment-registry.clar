;; Oscillating Knowledge Vault System

;; =========================================
;; Core System Constants and Error Handlers  
;; =========================================

(define-constant DIMENSIONAL-AUTHORITY tx-sender)

;; Protocol error response codes
(define-constant ERR-FRAGMENT-NONEXISTENT (err u301))
(define-constant ERR-DUPLICATE-FRAGMENT-ID (err u302))
(define-constant ERR-MALFORMED-FRAGMENT-LABEL (err u303))
(define-constant ERR-QUANTUM-SCALE-OUT-OF-BOUNDS (err u304))
(define-constant ERR-FORBIDDEN-RESEARCHER-ACCESS (err u305))
(define-constant ERR-INVALID-NEXUS-TARGET (err u306))
(define-constant ERR-AUTHORITY-RESTRICTED-OPERATION (err u307))
(define-constant ERR-ACCESS-CREDENTIALS-MISSING (err u308))

;; =========================================
;; Archive State Variables and Storage Maps
;; =========================================

;; Sequential identifier for quantum fragments
(define-data-var nexus-fragment-sequence uint u0)

;; Main repository for quantum research fragments
(define-map quantum-fragment-registry
  { fragment-id: uint }
  {
    label: (string-ascii 64),
    researcher: principal,
    quantum-scale: uint,
    timestamp-recorded: uint,
    research-summary: (string-ascii 128),
    dimensional-categories: (list 10 (string-ascii 32))
  }
)

;; Access control matrix for research visibility
(define-map researcher-access-matrix
  { fragment-id: uint, researcher: principal }
  { access-granted: bool }
)

;; =========================================
;; Internal Validation and Utility Functions
;; =========================================

;; Determines if quantum fragment exists in registry
(define-private (fragment-registry-contains? (fragment-id uint))
  (is-some (map-get? quantum-fragment-registry { fragment-id: fragment-id }))
)

;; Validates dimensional category descriptor format
(define-private (category-descriptor-valid? (category (string-ascii 32)))
  (and 
    (> (len category) u0)
    (< (len category) u33)
  )
)

;; Comprehensive validation for all dimensional categories
(define-private (validate-dimensional-categories? (categories (list 10 (string-ascii 32))))
  (and
    (> (len categories) u0)
    (<= (len categories) u10)
    (is-eq (len (filter category-descriptor-valid? categories)) (len categories))
  )
)

;; Confirms researcher attribution for specific fragment
(define-private (validate-researcher-ownership? (fragment-id uint) (researcher principal))
  (match (map-get? quantum-fragment-registry { fragment-id: fragment-id })
    fragment-data (is-eq (get researcher fragment-data) researcher)
    false
  )
)

;; Ensures string length parameters meet requirements
(define-private (text-field-length-valid? (text-content (string-ascii 64)) (minimum-chars uint) (maximum-chars uint))
  (and 
    (>= (len text-content) minimum-chars)
    (<= (len text-content) maximum-chars)
  )
)

;; Advances fragment sequence counter and returns previous value
(define-private (advance-fragment-sequence)
  (let ((current-sequence (var-get nexus-fragment-sequence)))
    (var-set nexus-fragment-sequence (+ current-sequence u1))
    (ok current-sequence)
  )
)

;; Extracts quantum scale measurement from fragment record
(define-private (extract-quantum-scale (fragment-id uint))
  (default-to u0 
    (get quantum-scale 
      (map-get? quantum-fragment-registry { fragment-id: fragment-id })
    )
  )
)

;; =========================================
;; Public Interface Functions
;; =========================================

;; Primary function for registering new quantum discoveries
(define-public (register-quantum-fragment (label (string-ascii 64)) (quantum-scale uint) (research-summary (string-ascii 128)) (dimensional-categories (list 10 (string-ascii 32))))
  (let
    (
      (new-fragment-id (+ (var-get nexus-fragment-sequence) u1))
    )
    ;; Execute comprehensive input validation sequence
    (asserts! (and (> (len label) u0) (< (len label) u65)) ERR-MALFORMED-FRAGMENT-LABEL)
    (asserts! (and (> quantum-scale u0) (< quantum-scale u1000000000)) ERR-QUANTUM-SCALE-OUT-OF-BOUNDS)
    (asserts! (and (> (len research-summary) u0) (< (len research-summary) u129)) ERR-MALFORMED-FRAGMENT-LABEL)
    (asserts! (validate-dimensional-categories? dimensional-categories) ERR-MALFORMED-FRAGMENT-LABEL)

    ;; Insert new fragment into quantum registry
    (map-insert quantum-fragment-registry
      { fragment-id: new-fragment-id }
      {
        label: label,
        researcher: tx-sender,
        quantum-scale: quantum-scale,
        timestamp-recorded: block-height,
        research-summary: research-summary,
        dimensional-categories: dimensional-categories
      }
    )

    ;; Establish researcher access permissions
    (map-insert researcher-access-matrix
      { fragment-id: new-fragment-id, researcher: tx-sender }
      { access-granted: true }
    )

    ;; Update global fragment sequence counter
    (var-set nexus-fragment-sequence new-fragment-id)
    (ok new-fragment-id)
  )
)

;; Validates fragment label formatting requirements
(define-public (validate-fragment-label-format (label (string-ascii 64)))
  (ok (and (> (len label) u0) (<= (len label) u64)))
)

;; Retrieves research summary for specified fragment
(define-public (retrieve-research-summary (fragment-id uint))
  (let
    (
      (fragment-data (unwrap! (map-get? quantum-fragment-registry { fragment-id: fragment-id }) ERR-FRAGMENT-NONEXISTENT))
    )
    (ok (get research-summary fragment-data))
  )
)

;; Confirms researcher access authorization for fragment
(define-public (confirm-researcher-authorization (fragment-id uint) (researcher principal))
  (let
    (
      (access-record (map-get? researcher-access-matrix { fragment-id: fragment-id, researcher: researcher }))
    )
    (ok (is-some access-record))
  )
)

;; Calculates total dimensional categories for fragment
(define-public (calculate-dimensional-count (fragment-id uint))
  (let
    (
      (fragment-data (unwrap! (map-get? quantum-fragment-registry { fragment-id: fragment-id }) ERR-FRAGMENT-NONEXISTENT))
    )
    (ok (len (get dimensional-categories fragment-data)))
  )
)

;; Transfers research attribution to different principal
(define-public (reassign-fragment-researcher (fragment-id uint) (target-researcher principal))
  (let
    (
      (fragment-data (unwrap! (map-get? quantum-fragment-registry { fragment-id: fragment-id }) ERR-FRAGMENT-NONEXISTENT))
    )
    (asserts! (fragment-registry-contains? fragment-id) ERR-FRAGMENT-NONEXISTENT)
    (asserts! (is-eq (get researcher fragment-data) tx-sender) ERR-FORBIDDEN-RESEARCHER-ACCESS)

    ;; Update researcher attribution in registry
    (map-set quantum-fragment-registry
      { fragment-id: fragment-id }
      (merge fragment-data { researcher: target-researcher })
    )
    (ok true)
  )
)

;; Comprehensive fragment record modification function
(define-public (modify-fragment-metadata (fragment-id uint) (updated-label (string-ascii 64)) (updated-scale uint) (updated-summary (string-ascii 128)) (updated-categories (list 10 (string-ascii 32))))
  (let
    (
      (fragment-data (unwrap! (map-get? quantum-fragment-registry { fragment-id: fragment-id }) ERR-FRAGMENT-NONEXISTENT))
    )
    ;; Verify permissions and validate all input parameters
    (asserts! (fragment-registry-contains? fragment-id) ERR-FRAGMENT-NONEXISTENT)
    (asserts! (is-eq (get researcher fragment-data) tx-sender) ERR-FORBIDDEN-RESEARCHER-ACCESS)
    (asserts! (and (> (len updated-label) u0) (< (len updated-label) u65)) ERR-MALFORMED-FRAGMENT-LABEL)
    (asserts! (and (> updated-scale u0) (< updated-scale u1000000000)) ERR-QUANTUM-SCALE-OUT-OF-BOUNDS)
    (asserts! (and (> (len updated-summary) u0) (< (len updated-summary) u129)) ERR-MALFORMED-FRAGMENT-LABEL)
    (asserts! (validate-dimensional-categories? updated-categories) ERR-MALFORMED-FRAGMENT-LABEL)

    ;; Execute comprehensive metadata update
    (map-set quantum-fragment-registry
      { fragment-id: fragment-id }
      (merge fragment-data { 
        label: updated-label, 
        quantum-scale: updated-scale, 
        research-summary: updated-summary, 
        dimensional-categories: updated-categories 
      })
    )
    (ok true)
  )
)

;; Permanently removes fragment from quantum archive
(define-public (purge-quantum-fragment (fragment-id uint))
  (let
    (
      (fragment-data (unwrap! (map-get? quantum-fragment-registry { fragment-id: fragment-id }) ERR-FRAGMENT-NONEXISTENT))
    )
    (asserts! (fragment-registry-contains? fragment-id) ERR-FRAGMENT-NONEXISTENT)
    (asserts! (is-eq (get researcher fragment-data) tx-sender) ERR-FORBIDDEN-RESEARCHER-ACCESS)

    ;; Execute permanent fragment removal from registry
    (map-delete quantum-fragment-registry { fragment-id: fragment-id })
    (ok true)
  )
)

