;; Silicon Art - Blockchain Gaming Ecosystem Smart Contract
;; A geological exploration and AI-powered NFT art generation platform

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-materials (err u103))
(define-constant err-not-authorized (err u104))
(define-constant err-invalid-price (err u105))
(define-constant err-listing-not-found (err u106))

;; Data Variables
(define-data-var last-token-id uint u0)
(define-data-var last-material-id uint u0)
(define-data-var platform-fee-percentage uint u250) ;; 2.5% (250/10000)

;; Material rarity levels
(define-constant rarity-common u1)
(define-constant rarity-uncommon u2)
(define-constant rarity-rare u3)
(define-constant rarity-epic u4)
(define-constant rarity-legendary u5)

;; NFT Data Maps
(define-map artworks
    uint
    {
        owner: principal,
        creator: principal,
        geological-location: (string-ascii 100),
        mineral-composition: (string-ascii 200),
        rarity: uint,
        created-at: uint,
        metadata-uri: (string-ascii 256)
    }
)

;; Material NFT Data
(define-map materials
    uint
    {
        owner: principal,
        discoverer: principal,
        silicon-type: (string-ascii 50),
        geological-coords: (string-ascii 100),
        purity-level: uint,
        rarity: uint,
        discovered-at: uint,
        is-consumed: bool
    }
)

;; Crafting Recipes - maps rarity level to required material count
(define-map crafting-requirements
    uint ;; target artwork rarity
    {
        materials-needed: uint,
        min-material-rarity: uint
    }
)

;; Marketplace Listings
(define-map artwork-listings
    uint ;; token-id
    {
        seller: principal,
        price: uint,
        listed-at: uint
    }
)

(define-map material-listings
    uint ;; material-id
    {
        seller: principal,
        price: uint,
        listed-at: uint
    }
)

;; User Statistics
(define-map explorer-stats
    principal
    {
        materials-discovered: uint,
        artworks-created: uint,
        total-trades: uint,
        reputation-score: uint
    }
)

;; Geological Database Contributions
(define-map geological-contributions
    principal
    {
        data-entries: uint,
        last-contribution: uint,
        rewards-earned: uint
    }
)

;; Read-only functions

(define-read-only (get-artwork (token-id uint))
    (map-get? artworks token-id)
)

(define-read-only (get-material (material-id uint))
    (map-get? materials material-id)
)

(define-read-only (get-artwork-listing (token-id uint))
    (map-get? artwork-listings token-id)
)

(define-read-only (get-material-listing (material-id uint))
    (map-get? material-listings material-id)
)

(define-read-only (get-explorer-stats (explorer principal))
    (default-to
        {materials-discovered: u0, artworks-created: u0, total-trades: u0, reputation-score: u0}
        (map-get? explorer-stats explorer)
    )
)

(define-read-only (get-last-token-id)
    (var-get last-token-id)
)

(define-read-only (get-last-material-id)
    (var-get last-material-id)
)

(define-read-only (get-platform-fee-percentage)
    (var-get platform-fee-percentage)
)

;; Private functions

(define-private (update-explorer-stats-discovery (explorer principal))
    (let
        (
            (current-stats (get-explorer-stats explorer))
        )
        (map-set explorer-stats explorer
            (merge current-stats {
                materials-discovered: (+ (get materials-discovered current-stats) u1),
                reputation-score: (+ (get reputation-score current-stats) u10)
            })
        )
    )
)

(define-private (update-explorer-stats-creation (creator principal))
    (let
        (
            (current-stats (get-explorer-stats creator))
        )
        (map-set explorer-stats creator
            (merge current-stats {
                artworks-created: (+ (get artworks-created current-stats) u1),
                reputation-score: (+ (get reputation-score current-stats) u50)
            })
        )
    )
)

(define-private (update-explorer-stats-trade (trader principal))
    (let
        (
            (current-stats (get-explorer-stats trader))
        )
        (map-set explorer-stats trader
            (merge current-stats {
                total-trades: (+ (get total-trades current-stats) u1),
                reputation-score: (+ (get reputation-score current-stats) u5)
            })
        )
    )
)

;; Public functions - Material Discovery

(define-public (discover-material 
    (silicon-type (string-ascii 50))
    (geological-coords (string-ascii 100))
    (purity-level uint)
    (rarity uint))
    (let
        (
            (material-id (+ (var-get last-material-id) u1))
        )
        (asserts! (<= rarity rarity-legendary) (err u107))
        (asserts! (> purity-level u0) (err u108))
        
        (map-set materials material-id
            {
                owner: tx-sender,
                discoverer: tx-sender,
                silicon-type: silicon-type,
                geological-coords: geological-coords,
                purity-level: purity-level,
                rarity: rarity,
                discovered-at: block-height,
                is-consumed: false
            }
        )
        
        (var-set last-material-id material-id)
        (update-explorer-stats-discovery tx-sender)
        
        (ok material-id)
    )
)

;; Crafting System

(define-public (craft-artwork
    (material-ids (list 10 uint))
    (geological-location (string-ascii 100))
    (mineral-composition (string-ascii 200))
    (metadata-uri (string-ascii 256)))
    (let
        (
            (token-id (+ (var-get last-token-id) u1))
            (avg-rarity (calculate-avg-rarity material-ids))
        )
        ;; Verify all materials exist and are owned by sender
        (asserts! (check-materials-ownership material-ids tx-sender) err-not-authorized)
        
        ;; Mark materials as consumed
        (map consume-material material-ids)
        
        ;; Create artwork NFT
        (map-set artworks token-id
            {
                owner: tx-sender,
                creator: tx-sender,
                geological-location: geological-location,
                mineral-composition: mineral-composition,
                rarity: avg-rarity,
                created-at: block-height,
                metadata-uri: metadata-uri
            }
        )
        
        (var-set last-token-id token-id)
        (update-explorer-stats-creation tx-sender)
        
        (ok token-id)
    )
)

(define-private (calculate-avg-rarity (material-ids (list 10 uint)))
    ;; Simplified: returns rare (u3) as default
    ;; In production, would calculate based on actual materials
    u3
)

(define-private (check-materials-ownership (material-ids (list 10 uint)) (owner principal))
    ;; Simplified ownership check
    true
)

(define-private (consume-material (material-id uint))
    (match (map-get? materials material-id)
        material (map-set materials material-id (merge material {is-consumed: true}))
        false
    )
)

;; Marketplace - Artwork Listings

(define-public (list-artwork (token-id uint) (price uint))
    (let
        (
            (artwork (unwrap! (map-get? artworks token-id) err-not-found))
        )
        (asserts! (is-eq (get owner artwork) tx-sender) err-not-authorized)
        (asserts! (> price u0) err-invalid-price)
        
        (map-set artwork-listings token-id
            {
                seller: tx-sender,
                price: price,
                listed-at: block-height
            }
        )
        
        (ok true)
    )
)

(define-public (unlist-artwork (token-id uint))
    (let
        (
            (listing (unwrap! (map-get? artwork-listings token-id) err-listing-not-found))
        )
        (asserts! (is-eq (get seller listing) tx-sender) err-not-authorized)
        
        (map-delete artwork-listings token-id)
        (ok true)
    )
)

(define-public (purchase-artwork (token-id uint))
    (let
        (
            (listing (unwrap! (map-get? artwork-listings token-id) err-listing-not-found))
            (artwork (unwrap! (map-get? artworks token-id) err-not-found))
            (price (get price listing))
            (seller (get seller listing))
            (platform-fee (/ (* price (var-get platform-fee-percentage)) u10000))
            (seller-amount (- price platform-fee))
        )
        ;; Transfer payment to seller
        (try! (stx-transfer? seller-amount tx-sender seller))
        
        ;; Transfer platform fee to contract owner
        (try! (stx-transfer? platform-fee tx-sender contract-owner))
        
        ;; Transfer artwork ownership
        (map-set artworks token-id (merge artwork {owner: tx-sender}))
        
        ;; Remove listing
        (map-delete artwork-listings token-id)
        
        ;; Update stats
        (update-explorer-stats-trade tx-sender)
        (update-explorer-stats-trade seller)
        
        (ok true)
    )
)

;; Marketplace - Material Listings

(define-public (list-material (material-id uint) (price uint))
    (let
        (
            (material (unwrap! (map-get? materials material-id) err-not-found))
        )
        (asserts! (is-eq (get owner material) tx-sender) err-not-authorized)
        (asserts! (not (get is-consumed material)) (err u109))
        (asserts! (> price u0) err-invalid-price)
        
        (map-set material-listings material-id
            {
                seller: tx-sender,
                price: price,
                listed-at: block-height
            }
        )
        
        (ok true)
    )
)

(define-public (purchase-material (material-id uint))
    (let
        (
            (listing (unwrap! (map-get? material-listings material-id) err-listing-not-found))
            (material (unwrap! (map-get? materials material-id) err-not-found))
            (price (get price listing))
            (seller (get seller listing))
            (platform-fee (/ (* price (var-get platform-fee-percentage)) u10000))
            (seller-amount (- price platform-fee))
        )
        ;; Transfer payment
        (try! (stx-transfer? seller-amount tx-sender seller))
        (try! (stx-transfer? platform-fee tx-sender contract-owner))
        
        ;; Transfer material ownership
        (map-set materials material-id (merge material {owner: tx-sender}))
        
        ;; Remove listing
        (map-delete material-listings material-id)
        
        ;; Update stats
        (update-explorer-stats-trade tx-sender)
        (update-explorer-stats-trade seller)
        
        (ok true)
    )
)

;; Geological Database Contributions

(define-public (contribute-geological-data (data-entries-count uint))
    (let
        (
            (current-contrib (default-to
                {data-entries: u0, last-contribution: u0, rewards-earned: u0}
                (map-get? geological-contributions tx-sender)
            ))
            (reward (* data-entries-count u100)) ;; 100 per entry as reputation
        )
        (map-set geological-contributions tx-sender
            {
                data-entries: (+ (get data-entries current-contrib) data-entries-count),
                last-contribution: block-height,
                rewards-earned: (+ (get rewards-earned current-contrib) reward)
            }
        )
        
        ;; Update reputation
        (let
            (
                (current-stats (get-explorer-stats tx-sender))
            )
            (map-set explorer-stats tx-sender
                (merge current-stats {
                    reputation-score: (+ (get reputation-score current-stats) reward)
                })
            )
        )
        
        (ok reward)
    )
)

;; Admin functions

(define-public (set-platform-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-fee u1000) (err u110)) ;; Max 10%
        (var-set platform-fee-percentage new-fee)
        (ok true)
    )
)

(define-public (set-crafting-requirement (rarity uint) (materials-needed uint) (min-rarity uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        (map-set crafting-requirements rarity
            {
                materials-needed: materials-needed,
                min-material-rarity: min-rarity
            }
        )
        
        (ok true)
    )
)

;; Initialize default crafting requirements
(map-set crafting-requirements rarity-common {materials-needed: u2, min-material-rarity: u1})
(map-set crafting-requirements rarity-uncommon {materials-needed: u3, min-material-rarity: u1})
(map-set crafting-requirements rarity-rare {materials-needed: u4, min-material-rarity: u2})
(map-set crafting-requirements rarity-epic {materials-needed: u5, min-material-rarity: u3})
(map-set crafting-requirements rarity-legendary {materials-needed: u7, min-material-rarity: u4})