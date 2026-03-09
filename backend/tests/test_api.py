from __future__ import annotations


def test_event_market_selection_flow(client) -> None:
    events_response = client.get("/api/v1/events", params={"bookmaker": "sportsbet", "limit": 5})
    assert events_response.status_code == 200
    events_payload = events_response.json()
    assert events_payload

    event_id = events_payload[0]["id"]
    event_detail = client.get(f"/api/v1/events/{event_id}")
    assert event_detail.status_code == 200

    markets_response = client.get(
        f"/api/v1/events/{event_id}/markets",
        params={"bookmaker": "sportsbet", "limit": 20},
    )
    assert markets_response.status_code == 200
    markets_payload = markets_response.json()
    assert markets_payload

    market_id = markets_payload[0]["id"]
    selections_response = client.get(
        f"/api/v1/markets/{market_id}/selections",
        params={"bookmaker": "sportsbet"},
    )
    assert selections_response.status_code == 200
    selections_payload = selections_response.json()
    assert selections_payload

    players_response = client.get("/api/v1/players/search", params={"q": "Zorko"})
    assert players_response.status_code == 200

    props_response = client.get("/api/v1/props/search", params={"bookmaker": "sportsbet", "q": "Zorko"})
    assert props_response.status_code == 200

    odds_response = client.get(
        "/api/v1/odds/search",
        params={"bookmaker": "sportsbet", "limit": 20},
    )
    assert odds_response.status_code == 200
    odds_payload = odds_response.json()
    assert odds_payload
    assert "diff_2025" in odds_payload[0]
    assert "diff_last_10" in odds_payload[0]

    stat_players_response = client.get("/api/v1/players/search", params={"q": "English"})
    assert stat_players_response.status_code == 200
    stat_players_payload = stat_players_response.json()
    assert stat_players_payload
    player_id = stat_players_payload[0]["id"]

    filters_response = client.get(f"/api/v1/players/{player_id}/stats/filters")
    assert filters_response.status_code == 200
    filters_payload = filters_response.json()
    assert filters_payload["seasons"]

    history_response = client.get(
        f"/api/v1/players/{player_id}/stats/history",
        params={"stat": "disposals", "seasons": filters_payload["seasons"][:1], "reference_line": 20.5, "line_mode": "single"},
    )
    assert history_response.status_code == 200

    summary_response = client.get(
        f"/api/v1/players/{player_id}/stats/summary",
        params={"stat": "disposals", "line_mode": "single", "reference_line": 20.5},
    )
    assert summary_response.status_code == 200
    summary_payload = summary_response.json()
    assert "sample_size" in summary_payload
