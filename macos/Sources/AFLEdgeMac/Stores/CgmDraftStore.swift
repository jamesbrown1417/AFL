import Foundation
import Observation

struct CgmDraftState: Equatable {
    var selectedBookmaker: String?
    var selectedEventIds: Set<Int> = []
    var selectedLegs: [DraftLeg] = []
    var comparisonResults: [CgmAgencyComparison] = []
    var latestError: String?
    var infoMessage: String?
}

@MainActor
@Observable
final class CgmDraftStore {
    private(set) var state = CgmDraftState()

    func selectBookmaker(_ bookmaker: String?) {
        guard state.selectedBookmaker != bookmaker else { return }
        state.selectedBookmaker = bookmaker
        state.selectedEventIds = []
        state.selectedLegs = []
        state.comparisonResults = []
        state.latestError = nil
        state.infoMessage = nil
    }

    func toggleEventSelection(_ eventId: Int) {
        var eventIds = state.selectedEventIds
        if eventIds.isEmpty {
            eventIds.insert(eventId)
        } else if !eventIds.insert(eventId).inserted {
            eventIds.remove(eventId)
        }
        let previousCount = state.selectedLegs.count
        let filteredLegs = eventIds.isEmpty
            ? state.selectedLegs
            : state.selectedLegs.filter { eventIds.contains($0.eventId) }
        state.selectedEventIds = eventIds
        state.selectedLegs = filteredLegs
        state.comparisonResults = []
        state.latestError = nil
        if filteredLegs.count != previousCount {
            state.infoMessage = "Draft trimmed to selected matches."
        }
    }

    func clearEventSelection() {
        state.selectedEventIds = []
        state.comparisonResults = []
        state.infoMessage = "Showing all matches."
    }

    func toggleLeg(_ leg: DraftLeg) -> DraftMutationResult {
        if state.selectedLegs.contains(where: { $0.selectionId == leg.selectionId }) {
            removeLeg(selectionId: leg.selectionId)
            return DraftMutationResult(applied: true, message: "Leg removed.")
        }
        if state.selectedLegs.contains(where: { $0.eventId == leg.eventId }) {
            state.latestError = "Cross-game multis allow one leg per match. Pick a different game."
            state.infoMessage = nil
            return DraftMutationResult(applied: false, message: state.latestError)
        }
        if let selectedBookmaker = state.selectedBookmaker, selectedBookmaker != leg.bookmaker {
            state.latestError = "A CGM draft can only contain one source bookmaker at a time."
            state.infoMessage = nil
            return DraftMutationResult(applied: false, message: state.latestError)
        }
        state.selectedBookmaker = leg.bookmaker
        state.selectedLegs.append(leg)
        state.selectedEventIds.remove(leg.eventId)
        state.comparisonResults = []
        state.latestError = nil
        state.infoMessage = "Leg added to CGM builder."
        AppLog.drafts.info("Added CGM leg \(leg.selectionId, privacy: .public)")
        return DraftMutationResult(applied: true, message: state.infoMessage)
    }

    func removeLeg(selectionId: Int) {
        state.selectedLegs.removeAll { $0.selectionId == selectionId }
        state.comparisonResults = []
        state.latestError = nil
        state.infoMessage = "Leg removed."
    }

    func clearDraft() {
        state.selectedLegs = []
        state.comparisonResults = []
        state.latestError = nil
        state.infoMessage = "Draft cleared."
        AppLog.drafts.info("Cleared CGM draft")
    }

    func setComparisons(_ comparisons: [CgmAgencyComparison]) {
        state.comparisonResults = comparisons
        state.latestError = nil
        state.infoMessage = comparisons.isEmpty ? "No agency currently offers the full combination." : "Comparison updated."
    }

    func setError(_ message: String?) {
        state.latestError = message
        state.comparisonResults = []
    }
}
