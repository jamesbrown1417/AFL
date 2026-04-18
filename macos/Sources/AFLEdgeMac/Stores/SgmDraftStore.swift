import Foundation
import Observation

@MainActor
@Observable
final class SgmDraftStore {
    private(set) var state = SgmDraftState()

    func addLeg(_ leg: DraftLeg) -> DraftMutationResult {
        let current = state
        if current.legs.contains(where: { $0.selectionId == leg.selectionId }) {
            return DraftMutationResult(applied: false, message: "Selection already added.")
        }
        if let eventId = current.eventId, eventId != leg.eventId {
            return DraftMutationResult(
                applied: false,
                message: "An SGM draft can only contain one event at a time."
            )
        }
        if let bookmaker = current.bookmaker, bookmaker != leg.bookmaker {
            return DraftMutationResult(
                applied: false,
                message: "An SGM draft can only contain one bookmaker at a time."
            )
        }
        state.bookmaker = leg.bookmaker
        state.eventId = leg.eventId
        state.eventLabel = leg.eventLabel
        state.legs.append(leg)
        state.latestQuote = nil
        state.latestComparisons = []
        state.latestError = nil
        AppLog.drafts.info("Added SGM leg \(leg.selectionId, privacy: .public)")
        return DraftMutationResult(applied: true, message: "Leg added to SGM builder.")
    }

    func removeLeg(selectionId: Int) {
        let updatedLegs = state.legs.filter { $0.selectionId != selectionId }
        if updatedLegs.isEmpty {
            state = SgmDraftState()
        } else {
            state.legs = updatedLegs
            state.latestQuote = nil
            state.latestComparisons = []
            state.latestError = nil
        }
        AppLog.drafts.info("Removed SGM leg \(selectionId, privacy: .public)")
    }

    func clear() {
        state = SgmDraftState()
        AppLog.drafts.info("Cleared SGM draft")
    }

    func setForceRefresh(_ forceRefresh: Bool) {
        state.forceRefresh = forceRefresh
    }

    func setQuote(_ quote: SgmQuoteResponse) {
        state.latestQuote = quote
        state.latestComparisons = []
        state.latestError = nil
    }

    func setComparisons(_ comparisons: [SgmAgencyComparison]) {
        state.latestQuote = comparisons.first?.quoteResponse
        state.latestComparisons = comparisons
        state.latestError = nil
    }

    func setError(_ message: String?) {
        state.latestError = message
        state.latestComparisons = []
    }
}

private extension SgmAgencyComparison {
    var quoteResponse: SgmQuoteResponse {
        SgmQuoteResponse(
            quoteId: quoteId,
            bookmaker: bookmaker,
            eventId: eventId,
            legs: legs,
            unadjustedPrice: unadjustedPrice,
            quotedPrice: quotedPrice,
            adjustmentFactor: adjustmentFactor,
            fromCache: fromCache,
            quotedAt: quotedAt,
            expiresAt: expiresAt,
            status: status
        )
    }
}
