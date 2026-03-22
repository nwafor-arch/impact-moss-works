;; ImpactMoss - Fair Trade Verification Ecosystem

;; This contract implements the core ImpactMoss platform:
;;   - Fair Trade DNA (product certification lifecycle)
;;   - Community validator nodes

;; ============================================================
;; CONSTANTS
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)

(define-constant ERR-NOT-AUTHORIZED        (err u100))
(define-constant ERR-NOT-FOUND             (err u101))
(define-constant ERR-ALREADY-EXISTS        (err u102))
(define-constant ERR-INVALID-PARAM         (err u103))
(define-constant ERR-INSUFFICIENT-BALANCE  (err u104))
(define-constant ERR-ALREADY-VALIDATED     (err u105))
(define-constant ERR-NOT-VALIDATOR         (err u106))
(define-constant ERR-PRODUCT-NOT-ACTIVE    (err u107))

;; Impact score thresholds (out of 100)
(define-constant SCORE-THRESHOLD-BRONZE u40)
(define-constant SCORE-THRESHOLD-SILVER u60)
(define-constant SCORE-THRESHOLD-GOLD   u80)

;; Premium distribution basis points (total must equal 10000)
;; 70% to producer, 20% to validators, 10% to community fund
(define-constant PRODUCER-SHARE-BPS    u7000)
(define-constant VALIDATOR-SHARE-BPS   u2000)
(define-constant COMMUNITY-SHARE-BPS   u1000)
(define-constant BPS-DIVISOR           u10000)

;; Minimum validators required before a product can be certified
(define-constant MIN-VALIDATIONS u3)

;; ============================================================
;; MOSS TOKEN (SIP-010 Fungible Token)
;; ============================================================

(define-fungible-token moss-token)

;; SIP-010 required read-only functions
(define-read-only (get-name)
  (ok "Moss Token"))

(define-read-only (get-symbol)
  (ok "MOSS"))

(define-read-only (get-decimals)
  (ok u6))

(define-read-only (get-balance (account principal))
  (ok (ft-get-balance moss-token account)))

(define-read-only (get-total-supply)
  (ok (ft-get-supply moss-token)))

(define-read-only (get-token-uri)
  (ok (some u"https://impactmoss.io/token-metadata.json")))

;; Transfer (SIP-010)
(define-public (transfer
    (amount uint)
    (sender principal)
    (recipient principal)
    (memo (optional (buff 34))))
  (begin
    (asserts! (is-eq tx-sender sender) ERR-NOT-AUTHORIZED)
    (try! (ft-transfer? moss-token amount sender recipient))
    (match memo m (begin (print m) true) true)
    (ok true)))

;; Mint - only contract owner
(define-public (mint (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (ft-mint? moss-token amount recipient)))

;; ============================================================
;; DATA MAPS AND VARIABLES
;; ============================================================

;; Global counters
(define-data-var next-product-id uint u1)
(define-data-var community-fund-balance uint u0)

;; Registered producers
;; { producer: principal } -> { name: (string-ascii 64), registered-at: uint, reputation: uint, active: bool }
(define-map producers
  { producer: principal }
  { name: (string-ascii 64), registered-at: uint, reputation: uint, active: bool })

;; Registered validators (community nodes)
;; { validator: principal } -> { name: (string-ascii 64), registered-at: uint, validations: uint, reputation: uint, active: bool }
(define-map validators
  { validator: principal }
  { name: (string-ascii 64), registered-at: uint, validations: uint, reputation: uint, active: bool })

;; Fair Trade DNA - each product record
;; { product-id: uint } -> { ... }
(define-map products
  { product-id: uint }
  {
    dna-hash: (buff 32),           ;; unique cryptographic identifier
    producer: principal,
    name: (string-ascii 128),
    description: (string-ascii 256),
    created-at: uint,
    labor-score: uint,             ;; 0-100
    env-score: uint,               ;; 0-100
    community-score: uint,         ;; 0-100
    composite-score: uint,         ;; weighted average 0-100
    validation-count: uint,
    certified: bool,
    active: bool,
    premium-pool: uint             ;; MOSS tokens allocated for distribution
  })

;; Validation records
;; { product-id: uint, validator: principal } -> { ... }
(define-map validations
  { product-id: uint, validator: principal }
  {
    labor-score: uint,
    env-score: uint,
    community-score: uint,
    notes-hash: (buff 32),         ;; hash of off-chain inspection notes
    validated-at: uint
  })

;; Track which validators have validated a product (for uniqueness)
(define-map validator-product-flag
  { product-id: uint, validator: principal }
  { validated: bool })

;; Supply chain events (up to 20 events per product tracked via a counter map)
(define-map supply-chain-event-count
  { product-id: uint }
  { count: uint })

(define-map supply-chain-events
  { product-id: uint, event-index: uint }
  {
    actor: principal,
    event-type: (string-ascii 32),  ;; e.g. "HARVEST", "PROCESS", "SHIP", "RETAIL"
    location-hash: (buff 32),
    timestamp: uint,
    data-hash: (buff 32)            ;; hash of off-chain IoT / sensor data
  })

;; Consumer micro-contributions
(define-map contributions
  { product-id: uint, contributor: principal }
  { total-contributed: uint })

;; ============================================================
;; PRODUCER MANAGEMENT
;; ============================================================

(define-public (register-producer (name (string-ascii 64)))
  (begin
    (asserts! (is-none (map-get? producers { producer: tx-sender })) ERR-ALREADY-EXISTS)
    (asserts! (> (len name) u0) ERR-INVALID-PARAM)
    (map-set producers
      { producer: tx-sender }
      { name: name, registered-at: block-height, reputation: u50, active: true })
    (ok true)))

(define-read-only (get-producer (producer principal))
  (map-get? producers { producer: producer }))

;; ============================================================
;; VALIDATOR MANAGEMENT
;; ============================================================

(define-public (register-validator (name (string-ascii 64)))
  (begin
    (asserts! (is-none (map-get? validators { validator: tx-sender })) ERR-ALREADY-EXISTS)
    (asserts! (> (len name) u0) ERR-INVALID-PARAM)
    (map-set validators
      { validator: tx-sender }
      { name: name, registered-at: block-height, validations: u0, reputation: u50, active: true })
    (ok true)))

(define-read-only (get-validator (validator principal))
  (map-get? validators { validator: validator }))

;; ============================================================
;; PRODUCT / FAIR TRADE DNA MANAGEMENT
;; ============================================================

;; Register a new product and mint its Fair Trade DNA
(define-public (register-product
    (dna-hash (buff 32))
    (name (string-ascii 128))
    (description (string-ascii 256)))
  (let
    ((producer-record (unwrap! (map-get? producers { producer: tx-sender }) ERR-NOT-AUTHORIZED))
     (product-id (var-get next-product-id)))
    (asserts! (get active producer-record) ERR-NOT-AUTHORIZED)
    (asserts! (> (len name) u0) ERR-INVALID-PARAM)
    (map-set products
      { product-id: product-id }
      {
        dna-hash: dna-hash,
        producer: tx-sender,
        name: name,
        description: description,
        created-at: block-height,
        labor-score: u0,
        env-score: u0,
        community-score: u0,
        composite-score: u0,
        validation-count: u0,
        certified: false,
        active: true,
        premium-pool: u0
      })
    (map-set supply-chain-event-count { product-id: product-id } { count: u0 })
    (var-set next-product-id (+ product-id u1))
    (ok product-id)))

(define-read-only (get-product (product-id uint))
  (map-get? products { product-id: product-id }))

;; ============================================================
;; COMMUNITY VALIDATION
;; ============================================================

;; A validator submits scores (0-100 each) after physical inspection.
;; Scores are averaged into the product's running composite score.
(define-public (submit-validation
    (product-id uint)
    (labor-score uint)
    (env-score uint)
    (community-score uint)
    (notes-hash (buff 32)))
  (let
    ((validator-record  (unwrap! (map-get? validators { validator: tx-sender }) ERR-NOT-VALIDATOR))
     (product-record    (unwrap! (map-get? products   { product-id: product-id }) ERR-NOT-FOUND))
     (already-validated (default-to { validated: false }
                          (map-get? validator-product-flag { product-id: product-id, validator: tx-sender }))))
    (asserts! (get active validator-record)  ERR-NOT-VALIDATOR)
    (asserts! (get active product-record)    ERR-PRODUCT-NOT-ACTIVE)
    (asserts! (not (get validated already-validated)) ERR-ALREADY-VALIDATED)
    (asserts! (<= labor-score u100)          ERR-INVALID-PARAM)
    (asserts! (<= env-score u100)            ERR-INVALID-PARAM)
    (asserts! (<= community-score u100)      ERR-INVALID-PARAM)

    ;; Record the individual validation
    (map-set validations
      { product-id: product-id, validator: tx-sender }
      { labor-score: labor-score, env-score: env-score,
        community-score: community-score, notes-hash: notes-hash,
        validated-at: block-height })
    (map-set validator-product-flag
      { product-id: product-id, validator: tx-sender }
      { validated: true })

    ;; Update running averages on the product using nested let blocks
    ;; (Clarity does not have let* - each binding layer depends on the previous)
    (let ((prev-count (get validation-count product-record)))
      (let ((new-count (+ prev-count u1)))
        (let ((new-labor (/ (+ (* (get labor-score     product-record) prev-count) labor-score)     new-count))
              (new-env   (/ (+ (* (get env-score       product-record) prev-count) env-score)       new-count))
              (new-comm  (/ (+ (* (get community-score product-record) prev-count) community-score) new-count)))
          (let ((new-composite (compute-composite new-labor new-env new-comm)))
            (let ((now-certified (and (>= new-count MIN-VALIDATIONS) (>= new-composite SCORE-THRESHOLD-BRONZE))))
              (map-set products
                { product-id: product-id }
                (merge product-record
                  { labor-score: new-labor, env-score: new-env,
                    community-score: new-comm, composite-score: new-composite,
                    validation-count: new-count, certified: now-certified }))
              ;; Increment validator stats and reputation
              (map-set validators
                { validator: tx-sender }
                (merge validator-record
                  { validations: (+ (get validations validator-record) u1),
                    reputation:  (min u100 (+ (get reputation validator-record) u2)) })))))))

    (ok true)))

(define-read-only (get-validation (product-id uint) (validator principal))
  (map-get? validations { product-id: product-id, validator: validator }))

;; ============================================================
;; SUPPLY CHAIN EVENT LOGGING
;; ============================================================

;; Producers and validators can append supply chain events to a product's DNA history.
(define-public (log-supply-chain-event
    (product-id uint)
    (event-type (string-ascii 32))
    (location-hash (buff 32))
    (data-hash (buff 32)))
  (let
    ((product-record (unwrap! (map-get? products { product-id: product-id }) ERR-NOT-FOUND))
     (event-counter  (default-to { count: u0 }
                       (map-get? supply-chain-event-count { product-id: product-id })))
     (idx (get count event-counter)))
    (asserts! (get active product-record) ERR-PRODUCT-NOT-ACTIVE)
    ;; Only the producer or a registered validator may log events
    (asserts!
      (or
        (is-eq tx-sender (get producer product-record))
        (is-some (map-get? validators { validator: tx-sender })))
      ERR-NOT-AUTHORIZED)
    (asserts! (< idx u20) ERR-INVALID-PARAM)  ;; cap at 20 events

    (map-set supply-chain-events
      { product-id: product-id, event-index: idx }
      { actor: tx-sender, event-type: event-type,
        location-hash: location-hash, timestamp: block-height, data-hash: data-hash })
    (map-set supply-chain-event-count
      { product-id: product-id }
      { count: (+ idx u1) })
    (ok idx)))

(define-read-only (get-supply-chain-event (product-id uint) (event-index uint))
  (map-get? supply-chain-events { product-id: product-id, event-index: event-index }))

(define-read-only (get-event-count (product-id uint))
  (default-to { count: u0 } (map-get? supply-chain-event-count { product-id: product-id })))

;; ============================================================
;; PREMIUM POOL AND DISTRIBUTION (SMART CONTRACT PAYMENTS)
;; ============================================================

;; Anyone can fund a product's premium pool with MOSS tokens.
(define-public (fund-premium-pool (product-id uint) (amount uint))
  (let
    ((product-record (unwrap! (map-get? products { product-id: product-id }) ERR-NOT-FOUND)))
    (asserts! (get active product-record) ERR-PRODUCT-NOT-ACTIVE)
    (asserts! (> amount u0) ERR-INVALID-PARAM)
    (try! (ft-transfer? moss-token amount tx-sender (as-contract tx-sender)))
    (map-set products
      { product-id: product-id }
      (merge product-record { premium-pool: (+ (get premium-pool product-record) amount) }))
    (ok true)))

;; Distribute the premium pool for a certified product.
;; 70% to producer, 20% split equally among validators, 10% to community fund.
;; Can only be called by the contract owner (or could be automated via a trusted oracle).
(define-public (distribute-premium (product-id uint))
  (let
    ((product-record  (unwrap! (map-get? products { product-id: product-id }) ERR-NOT-FOUND))
     (pool            (get premium-pool product-record))
     (val-count       (get validation-count product-record)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (get certified product-record)   ERR-PRODUCT-NOT-ACTIVE)
    (asserts! (> pool u0)                      ERR-INVALID-PARAM)
    (asserts! (> val-count u0)                 ERR-INVALID-PARAM)

    (let ((producer-share   (/ (* pool PRODUCER-SHARE-BPS)  BPS-DIVISOR))
          (validator-total  (/ (* pool VALIDATOR-SHARE-BPS) BPS-DIVISOR))
          (community-share  (/ (* pool COMMUNITY-SHARE-BPS) BPS-DIVISOR)))
      (let ((per-validator (/ validator-total val-count)))

        ;; Pay producer
        (try! (as-contract (ft-transfer? moss-token producer-share tx-sender (get producer product-record))))

        ;; Accrue community share to the on-chain fund variable
        (var-set community-fund-balance (+ (var-get community-fund-balance) community-share))

        ;; Zero out the pool
        (map-set products
          { product-id: product-id }
          (merge product-record { premium-pool: u0 }))

        ;; Return per-validator share so the caller can distribute off-chain
        ;; (full per-validator distribution would require iterating over all validators,
        ;; which is not supported in Clarity without a list; callers should use
        ;; distribute-validator-share for each validator individually)
        (ok { producer-share: producer-share,
              per-validator-share: per-validator,
              community-share: community-share })))))

;; Called once per validator after distribute-premium to transfer their share.
(define-public (distribute-validator-share (product-id uint) (validator principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? validations { product-id: product-id, validator: validator }))
              ERR-NOT-FOUND)
    (as-contract (ft-transfer? moss-token amount tx-sender validator))))

;; Contract owner can disperse accumulated community fund to a recipient (e.g. a DAO treasury).
(define-public (disperse-community-fund (recipient principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= amount (var-get community-fund-balance)) ERR-INSUFFICIENT-BALANCE)
    (var-set community-fund-balance (- (var-get community-fund-balance) amount))
    (as-contract (ft-transfer? moss-token amount tx-sender recipient))))

(define-read-only (get-community-fund-balance)
  (var-get community-fund-balance))

;; ============================================================
;; CONSUMER MICRO-CONTRIBUTIONS
;; ============================================================

;; Consumers can send MOSS tokens directly as micro-contributions to a product's premium pool.
(define-public (consumer-contribute (product-id uint) (amount uint))
  (let
    ((product-record (unwrap! (map-get? products { product-id: product-id }) ERR-NOT-FOUND))
     (existing       (default-to { total-contributed: u0 }
                       (map-get? contributions { product-id: product-id, contributor: tx-sender }))))
    (asserts! (get active product-record) ERR-PRODUCT-NOT-ACTIVE)
    (asserts! (> amount u0) ERR-INVALID-PARAM)
    (try! (ft-transfer? moss-token amount tx-sender (as-contract tx-sender)))
    (map-set products
      { product-id: product-id }
      (merge product-record { premium-pool: (+ (get premium-pool product-record) amount) }))
    (map-set contributions
      { product-id: product-id, contributor: tx-sender }
      { total-contributed: (+ (get total-contributed existing) amount) })
    (ok true)))

(define-read-only (get-contribution (product-id uint) (contributor principal))
  (map-get? contributions { product-id: product-id, contributor: contributor }))

;; ============================================================
;; REPUTATION SCORING
;; ============================================================

;; Reward a producer's reputation when their product achieves Gold certification.
(define-public (reward-producer-reputation (product-id uint))
  (let
    ((product-record  (unwrap! (map-get? products { product-id: product-id }) ERR-NOT-FOUND))
     (producer-record (unwrap! (map-get? producers { producer: (get producer product-record) }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (>= (get composite-score product-record) SCORE-THRESHOLD-GOLD) ERR-INVALID-PARAM)
    (map-set producers
      { producer: (get producer product-record) }
      (merge producer-record
        { reputation: (min u100 (+ (get reputation producer-record) u5)) }))
    (ok true)))

(define-read-only (get-certification-tier (product-id uint))
  (match (map-get? products { product-id: product-id })
    product (let ((score (get composite-score product)))
              (if (>= score SCORE-THRESHOLD-GOLD)
                (ok "GOLD")
                (if (>= score SCORE-THRESHOLD-SILVER)
                  (ok "SILVER")
                  (if (>= score SCORE-THRESHOLD-BRONZE)
                    (ok "BRONZE")
                    (ok "UNRATED")))))
    ERR-NOT-FOUND))

;; ============================================================
;; ADMIN UTILITIES
;; ============================================================

(define-public (deactivate-product (product-id uint))
  (let ((product-record (unwrap! (map-get? products { product-id: product-id }) ERR-NOT-FOUND)))
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER)
                  (is-eq tx-sender (get producer product-record))) ERR-NOT-AUTHORIZED)
    (map-set products { product-id: product-id }
             (merge product-record { active: false }))
    (ok true)))

(define-public (deactivate-validator (validator principal))
  (let ((validator-record (unwrap! (map-get? validators { validator: validator }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set validators { validator: validator }
             (merge validator-record { active: false }))
    (ok true)))

(define-read-only (get-next-product-id)
  (var-get next-product-id))

;; ============================================================
;; HELPERS
;; ============================================================

;; Weighted composite score: 40% labor, 35% env, 25% community
(define-private (compute-composite (labor uint) (env uint) (comm uint))
  (/ (+ (* labor u40) (* env u35) (* comm u25)) u100))

;; Safe min for uint
(define-private (min (a uint) (b uint))
  (if (<= a b) a b))
