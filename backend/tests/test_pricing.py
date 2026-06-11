from __future__ import annotations

import json
from collections import defaultdict

from app.bookmakers.base import ResolvedLeg
from app.bookmakers.pointsbet import PointsbetAdapter
from app.bookmakers.sportsbet import SportsbetAdapter
from app.bookmakers.tab import TabAdapter
from app.config import get_settings
from app.db.duckdb import connection, fetch_all


def _selection_ids_by_event(imported_settings, bookmaker_code: str) -> dict[int, list[int]]:
    with connection(settings=imported_settings) as conn:
        rows = fetch_all(
            conn,
            """
            SELECT m.event_id, s.selection_id
            FROM selections s
            JOIN markets m ON m.market_id = s.market_id
            JOIN selection_bookmaker_meta sbm ON sbm.selection_id = s.selection_id
            JOIN bookmakers b ON b.bookmaker_id = sbm.bookmaker_id
            WHERE b.code = ? AND sbm.sgm_eligible = TRUE
            ORDER BY m.event_id, s.selection_id
            """,
            [bookmaker_code],
        )

    by_event: dict[int, list[int]] = defaultdict(list)
    for row in rows:
        by_event[int(row["event_id"])].append(int(row["selection_id"]))
    return by_event


def test_tab_adapter_builds_expected_request() -> None:
    settings = get_settings()
    adapter = TabAdapter(settings)
    request_spec = adapter.build_request(
        [
            ResolvedLeg(
                selection_id=1,
                event_id=10,
                market_id=101,
                bookmaker_code="tab",
                market_type_code="player_disposals",
                selection_type="over",
                label="Player One Over 24.5",
                base_price=1.9,
                external_event_id=None,
                external_competition_id=None,
                external_market_id=None,
                external_selection_id="12345",
                event_payload_meta={},
                selection_payload_meta={},
            ),
            ResolvedLeg(
                selection_id=2,
                event_id=10,
                market_id=102,
                bookmaker_code="tab",
                market_type_code="player_goals",
                selection_type="under",
                label="Player Two Under 1.5",
                base_price=2.1,
                external_event_id=None,
                external_competition_id=None,
                external_market_id=None,
                external_selection_id="67890",
                event_payload_meta={},
                selection_payload_meta={},
            ),
        ]
    )

    assert request_spec["url"] == settings.tab_quote_url
    # The v2 adapter sends a pre-serialised body with browser-like headers.
    assert request_spec["headers"]["content-type"] == "application/json;charset=UTF-8"
    assert request_spec["headers"]["origin"] == settings.tab_origin
    payload = json.loads(request_spec["body"])
    assert payload["clientDetails"] == {
        "jurisdiction": settings.tab_jurisdiction,
        "channel": settings.tab_channel,
    }
    assert payload["returnValidationMatrix"] is True
    propositions = payload["bets"][0]["legs"][0]["propositions"]
    assert propositions == [
        {"type": "WIN", "propositionId": 12345},
        {"type": "WIN", "propositionId": 67890},
    ]


def test_pointsbet_adapter_builds_expected_request() -> None:
    settings = get_settings()
    adapter = PointsbetAdapter(settings)
    request_spec = adapter.build_request(
        [
            ResolvedLeg(
                selection_id=1,
                event_id=10,
                market_id=101,
                bookmaker_code="pointsbet",
                market_type_code="player_disposals",
                selection_type="over",
                label="Player One Over 24.5",
                base_price=1.9,
                external_event_id="2625226",
                external_competition_id=None,
                external_market_id="104673899",
                external_selection_id="1",
                event_payload_meta={},
                selection_payload_meta={},
            ),
            ResolvedLeg(
                selection_id=2,
                event_id=10,
                market_id=102,
                bookmaker_code="pointsbet",
                market_type_code="player_goals",
                selection_type="over",
                label="Player Two Over 1.5",
                base_price=2.1,
                external_event_id="2625226",
                external_competition_id=None,
                external_market_id="104673900",
                external_selection_id="8",
                event_payload_meta={},
                selection_payload_meta={},
            ),
        ]
    )

    assert request_spec["url"] == settings.pointsbet_quote_url
    assert request_spec["json"] == {
        "EventKey": 2625226,
        "SelectedOutcomes": [
            {"MarketKey": 104673899, "OutcomeKey": 1},
            {"MarketKey": 104673900, "OutcomeKey": 8},
        ],
    }


def test_pricing_route_uses_cache(client, imported_settings, monkeypatch) -> None:
    by_event = _selection_ids_by_event(imported_settings, "sportsbet")
    event_id, selection_ids = next((event_id, ids) for event_id, ids in by_event.items() if len(ids) >= 2)
    call_count = {"send": 0}

    async def fake_send(self, client_arg, request_spec):
        del self, client_arg, request_spec
        call_count["send"] += 1
        return {"price": {"numerator": 7, "denominator": 2}}

    monkeypatch.setattr(SportsbetAdapter, "send", fake_send)

    payload = {
        "bookmaker": "sportsbet",
        "event_id": event_id,
        "legs": [{"selection_id": selection_ids[0]}, {"selection_id": selection_ids[1]}],
        "force_refresh": False,
    }

    first_response = client.post("/api/v1/pricing/sgm", json=payload)
    assert first_response.status_code == 200
    first_payload = first_response.json()
    assert first_payload["from_cache"] is False
    assert call_count["send"] == 1

    second_response = client.post("/api/v1/pricing/sgm", json=payload)
    assert second_response.status_code == 200
    second_payload = second_response.json()
    assert second_payload["from_cache"] is True
    assert call_count["send"] == 1

    quote_response = client.get(f"/api/v1/quotes/{first_payload['quote_id']}")
    assert quote_response.status_code == 200


def test_pointsbet_pricing_route(client, imported_settings, monkeypatch) -> None:
    by_event = _selection_ids_by_event(imported_settings, "pointsbet")
    event_id, selection_ids = next((event_id, ids) for event_id, ids in by_event.items() if len(ids) >= 2)

    async def fake_send(self, client_arg, request_spec):
        del self, client_arg
        assert request_spec["json"]["SelectedOutcomes"]
        return {"price": 4.2}

    monkeypatch.setattr(PointsbetAdapter, "send", fake_send)

    payload = {
        "bookmaker": "pointsbet",
        "event_id": event_id,
        "legs": [{"selection_id": selection_ids[0]}, {"selection_id": selection_ids[1]}],
        "force_refresh": True,
    }
    response = client.post("/api/v1/pricing/sgm", json=payload)
    assert response.status_code == 200
    body = response.json()
    assert body["bookmaker"] == "pointsbet"
    assert body["quoted_price"] == 4.2


def test_bet365_pricing_route_uses_formula(client, imported_settings) -> None:
    by_event = _selection_ids_by_event(imported_settings, "bet365")
    event_id, selection_ids = next((event_id, ids) for event_id, ids in by_event.items() if len(ids) >= 2)

    payload = {
        "bookmaker": "bet365",
        "event_id": event_id,
        "legs": [{"selection_id": selection_ids[0]}, {"selection_id": selection_ids[1]}],
        "force_refresh": True,
    }
    response = client.post("/api/v1/pricing/sgm", json=payload)
    assert response.status_code == 200
    body = response.json()
    assert body["bookmaker"] == "bet365"
    expected = round(1 / (0.004 + (1 / body["unadjusted_price"])), 2)
    assert body["quoted_price"] == expected
