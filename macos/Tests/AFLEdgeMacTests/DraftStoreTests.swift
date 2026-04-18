import XCTest
@testable import AFLEdgeMac

@MainActor
final class DraftStoreTests: XCTestCase {
    func testSgmDraftRejectsDifferentEventAndBookmaker() {
        let store = SgmDraftStore()
        let first = draftLeg(selectionId: 1, eventId: 100, bookmaker: "sportsbet")
        let differentEvent = draftLeg(selectionId: 2, eventId: 200, bookmaker: "sportsbet")
        let differentBookmaker = draftLeg(selectionId: 3, eventId: 100, bookmaker: "tab")

        XCTAssertTrue(store.addLeg(first).applied)
        XCTAssertFalse(store.addLeg(differentEvent).applied)
        XCTAssertFalse(store.addLeg(differentBookmaker).applied)
        XCTAssertEqual(store.state.legs.map(\.selectionId), [1])
    }

    func testSgmDraftClearsWhenLastLegRemoved() {
        let store = SgmDraftStore()

        _ = store.addLeg(draftLeg(selectionId: 1, eventId: 100, bookmaker: "sportsbet"))
        store.removeLeg(selectionId: 1)

        XCTAssertTrue(store.state.legs.isEmpty)
        XCTAssertNil(store.state.eventId)
        XCTAssertNil(store.state.bookmaker)
    }

    func testCgmDraftRejectsSecondLegFromSameEvent() {
        let store = CgmDraftStore()
        store.selectBookmaker("sportsbet")

        XCTAssertTrue(store.toggleLeg(draftLeg(selectionId: 1, eventId: 100, bookmaker: "sportsbet")).applied)
        XCTAssertFalse(store.toggleLeg(draftLeg(selectionId: 2, eventId: 100, bookmaker: "sportsbet")).applied)
        XCTAssertTrue(store.toggleLeg(draftLeg(selectionId: 3, eventId: 200, bookmaker: "sportsbet")).applied)
        XCTAssertEqual(store.state.selectedLegs.map(\.selectionId), [1, 3])
    }

    func testCgmEventFilterTrimsDraft() {
        let store = CgmDraftStore()
        store.selectBookmaker("sportsbet")
        _ = store.toggleLeg(draftLeg(selectionId: 1, eventId: 100, bookmaker: "sportsbet"))
        _ = store.toggleLeg(draftLeg(selectionId: 2, eventId: 200, bookmaker: "sportsbet"))

        store.toggleEventSelection(100)

        XCTAssertEqual(store.state.selectedLegs.map(\.selectionId), [1])
        XCTAssertEqual(store.state.infoMessage, "Draft trimmed to selected matches.")
    }

    private func draftLeg(selectionId: Int, eventId: Int, bookmaker: String) -> DraftLeg {
        DraftLeg(
            selectionId: selectionId,
            eventId: eventId,
            eventLabel: "Event \(eventId)",
            bookmaker: bookmaker,
            label: "Player \(selectionId) Over 19.5",
            marketTypeCode: "player_disposals",
            selectionType: "over",
            basePrice: 1.90
        )
    }
}
