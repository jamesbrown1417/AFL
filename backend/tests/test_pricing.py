from __future__ import annotations

from collections import defaultdict

from app.bookmakers.sportsbet import SportsbetAdapter
from app.db.duckdb import connection, fetch_all


def test_pricing_route_uses_cache(client, imported_settings, monkeypatch) -> None:
    with connection(settings=imported_settings) as conn:
        rows = fetch_all(
            conn,
            """
            SELECT m.event_id, s.selection_id
            FROM selections s
            JOIN markets m ON m.market_id = s.market_id
            JOIN selection_bookmaker_meta sbm ON sbm.selection_id = s.selection_id
            JOIN bookmakers b ON b.bookmaker_id = sbm.bookmaker_id
            WHERE b.code = 'sportsbet' AND sbm.sgm_eligible = TRUE
            ORDER BY m.event_id, s.selection_id
            """,
        )

    by_event: dict[int, list[int]] = defaultdict(list)
    for row in rows:
        by_event[int(row["event_id"])].append(int(row["selection_id"]))

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
