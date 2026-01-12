;; Media Basin Studio - Decentralized Media Licensing Contract
;; A smart contract for managing media assets, licensing, and revenue distribution

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-invalid-price (err u104))
(define-constant err-insufficient-funds (err u105))

;; Data Variables
(define-data-var platform-fee-percentage uint u5) ;; 5% platform fee

;; Data Maps
;; Media Asset Registry
(define-map media-assets
  { asset-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    fingerprint: (string-ascii 64),
    license-price: uint,
    royalty-percentage: uint,
    total-revenue: uint,
    active: bool
  }
)

;; License Purchases
(define-map licenses
  { license-id: uint }
  {
    asset-id: uint,
    licensee: principal,
    purchase-price: uint,
    timestamp: uint,
    active: bool
  }
)

;; Derivative Works Tracking
(define-map derivatives
  { derivative-id: uint }
  {
    original-asset-id: uint,
    derivative-asset-id: uint,
    creator: principal,
    royalty-split: uint
  }
)

;; Creator Collectives
(define-map collectives
  { collective-id: uint }
  {
    name: (string-ascii 50),
    admin: principal,
    member-count: uint,
    total-revenue: uint
  }
)

(define-map collective-members
  { collective-id: uint, member: principal }
  { share-percentage: uint, joined-at: uint }
)

;; Counters
(define-data-var next-asset-id uint u1)
(define-data-var next-license-id uint u1)
(define-data-var next-derivative-id uint u1)
(define-data-var next-collective-id uint u1)

;; Read-only functions
(define-read-only (get-media-asset (asset-id uint))
  (map-get? media-assets { asset-id: asset-id })
)

(define-read-only (get-license (license-id uint))
  (map-get? licenses { license-id: license-id })
)

(define-read-only (get-platform-fee)
  (var-get platform-fee-percentage)
)

(define-read-only (get-collective (collective-id uint))
  (map-get? collectives { collective-id: collective-id })
)

(define-read-only (get-collective-member (collective-id uint) (member principal))
  (map-get? collective-members { collective-id: collective-id, member: member })
)

;; Private functions
(define-private (calculate-platform-fee (amount uint))
  (/ (* amount (var-get platform-fee-percentage)) u100)
)

(define-private (calculate-creator-payment (amount uint))
  (- amount (calculate-platform-fee amount))
)

;; Public functions

;; Register a new media asset
(define-public (register-media-asset 
  (title (string-ascii 100))
  (fingerprint (string-ascii 64))
  (license-price uint)
  (royalty-percentage uint))
  (let
    (
      (asset-id (var-get next-asset-id))
    )
    (asserts! (> license-price u0) err-invalid-price)
    (asserts! (<= royalty-percentage u100) err-invalid-price)
    
    (map-set media-assets
      { asset-id: asset-id }
      {
        creator: tx-sender,
        title: title,
        fingerprint: fingerprint,
        license-price: license-price,
        royalty-percentage: royalty-percentage,
        total-revenue: u0,
        active: true
      }
    )
    
    (var-set next-asset-id (+ asset-id u1))
    (ok asset-id)
  )
)

;; Purchase a license for a media asset
(define-public (purchase-license (asset-id uint))
  (let
    (
      (asset (unwrap! (map-get? media-assets { asset-id: asset-id }) err-not-found))
      (license-id (var-get next-license-id))
      (price (get license-price asset))
      (creator (get creator asset))
      (platform-fee (calculate-platform-fee price))
      (creator-payment (calculate-creator-payment price))
    )
    (asserts! (get active asset) err-not-found)
    
    ;; Transfer payment to creator
    (try! (stx-transfer? creator-payment tx-sender creator))
    
    ;; Transfer platform fee to contract owner
    (try! (stx-transfer? platform-fee tx-sender contract-owner))
    
    ;; Record the license
    (map-set licenses
      { license-id: license-id }
      {
        asset-id: asset-id,
        licensee: tx-sender,
        purchase-price: price,
        timestamp: block-height,
        active: true
      }
    )
    
    ;; Update asset revenue
    (map-set media-assets
      { asset-id: asset-id }
      (merge asset { total-revenue: (+ (get total-revenue asset) price) })
    )
    
    (var-set next-license-id (+ license-id u1))
    (ok license-id)
  )
)

;; Register derivative work
(define-public (register-derivative 
  (original-asset-id uint)
  (derivative-asset-id uint)
  (royalty-split uint))
  (let
    (
      (original (unwrap! (map-get? media-assets { asset-id: original-asset-id }) err-not-found))
      (derivative (unwrap! (map-get? media-assets { asset-id: derivative-asset-id }) err-not-found))
      (derivative-id (var-get next-derivative-id))
    )
    (asserts! (is-eq tx-sender (get creator derivative)) err-unauthorized)
    (asserts! (<= royalty-split u100) err-invalid-price)
    
    (map-set derivatives
      { derivative-id: derivative-id }
      {
        original-asset-id: original-asset-id,
        derivative-asset-id: derivative-asset-id,
        creator: tx-sender,
        royalty-split: royalty-split
      }
    )
    
    (var-set next-derivative-id (+ derivative-id u1))
    (ok derivative-id)
  )
)

;; Create a creator collective
(define-public (create-collective (name (string-ascii 50)))
  (let
    (
      (collective-id (var-get next-collective-id))
    )
    (map-set collectives
      { collective-id: collective-id }
      {
        name: name,
        admin: tx-sender,
        member-count: u1,
        total-revenue: u0
      }
    )
    
    ;; Add creator as first member with 100% share
    (map-set collective-members
      { collective-id: collective-id, member: tx-sender }
      { share-percentage: u100, joined-at: block-height }
    )
    
    (var-set next-collective-id (+ collective-id u1))
    (ok collective-id)
  )
)

;; Add member to collective
(define-public (add-collective-member 
  (collective-id uint)
  (member principal)
  (share-percentage uint))
  (let
    (
      (collective (unwrap! (map-get? collectives { collective-id: collective-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get admin collective)) err-unauthorized)
    (asserts! (<= share-percentage u100) err-invalid-price)
    
    (map-set collective-members
      { collective-id: collective-id, member: member }
      { share-percentage: share-percentage, joined-at: block-height }
    )
    
    (map-set collectives
      { collective-id: collective-id }
      (merge collective { member-count: (+ (get member-count collective) u1) })
    )
    
    (ok true)
  )
)

;; Update asset status
(define-public (toggle-asset-status (asset-id uint))
  (let
    (
      (asset (unwrap! (map-get? media-assets { asset-id: asset-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator asset)) err-unauthorized)
    
    (map-set media-assets
      { asset-id: asset-id }
      (merge asset { active: (not (get active asset)) })
    )
    
    (ok true)
  )
)

;; Update platform fee (owner only)
(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u20) err-invalid-price) ;; Max 20% fee
    (var-set platform-fee-percentage new-fee)
    (ok true)
  )
)
