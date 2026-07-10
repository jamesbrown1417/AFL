from __future__ import annotations


def test_event_market_selection_flow(client) -> None:
    data_status_response = client.get("/api/v1/data/status")
    assert data_status_response.status_code == 200
    data_status_payload = data_status_response.json()
    assert data_status_payload["sections"]

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

    all_players_response = client.get("/api/v1/players/search", params={"limit": 25})
    assert all_players_response.status_code == 200
    assert all_players_response.json()

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
    assert "is_best_price" in odds_payload[0]
    assert "next_best_prob_diff" in odds_payload[0]
    assert "weather" in odds_payload[0]
    player_rows = [row for row in odds_payload if row["player"] is not None]
    assert player_rows
    player_id = player_rows[0]["player"]["id"]

    sorted_odds_response = client.get(
        "/api/v1/odds/search",
        params={
            "bookmaker": "sportsbet",
            "limit": 50,
            "sort_by": "diff_last_10",
            "sort_dir": "desc",
        },
    )
    assert sorted_odds_response.status_code == 200
    sorted_odds_payload = sorted_odds_response.json()
    ranked_diffs = [
        row["diff_last_10"]
        for row in sorted_odds_payload
        if row["diff_last_10"] is not None
    ]
    if len(ranked_diffs) >= 2:
        assert ranked_diffs == sorted(ranked_diffs, reverse=True)

    market_diff_sorted_response = client.get(
        "/api/v1/odds/search",
        params={
            "bookmaker": "sportsbet",
            "scope": "player",
            "limit": 50,
            "sort_by": "next_best_prob_diff",
            "sort_dir": "desc",
        },
    )
    assert market_diff_sorted_response.status_code == 200
    market_diff_payload = market_diff_sorted_response.json()
    ranked_market_diffs = [
        row["next_best_prob_diff"]
        for row in market_diff_payload
        if row["next_best_prob_diff"] is not None
    ]
    if len(ranked_market_diffs) >= 2:
        assert ranked_market_diffs == sorted(ranked_market_diffs, reverse=True)

    match_odds_response = client.get(
        "/api/v1/odds/search",
        params={
            "bookmaker": "sportsbet",
            "scope": "match",
            "limit": 20,
            "sort_by": "start_time",
            "sort_dir": "asc",
        },
    )
    assert match_odds_response.status_code == 200
    match_odds_payload = match_odds_response.json()
    assert match_odds_payload
    assert all(row["player"] is None for row in match_odds_payload)

    include_player_response = client.get(
        "/api/v1/odds/search",
        params={
            "bookmaker": "sportsbet",
            "scope": "player",
            "include_player_id": player_id,
            "limit": 50,
        },
    )
    assert include_player_response.status_code == 200
    include_player_payload = include_player_response.json()
    assert include_player_payload
    assert all(row["player"]["id"] == player_id for row in include_player_payload if row["player"] is not None)

    exclude_player_response = client.get(
        "/api/v1/odds/search",
        params={
            "bookmaker": "sportsbet",
            "scope": "player",
            "exclude_player_id": player_id,
            "limit": 50,
        },
    )
    assert exclude_player_response.status_code == 200
    exclude_player_payload = exclude_player_response.json()
    assert exclude_player_payload
    assert all(row["player"]["id"] != player_id for row in exclude_player_payload if row["player"] is not None)

    price_filtered_response = client.get(
        "/api/v1/odds/search",
        params={
            "bookmaker": "sportsbet",
            "scope": "player",
            "min_price": 1.5,
            "max_price": 3.5,
            "limit": 50,
        },
    )
    assert price_filtered_response.status_code == 200
    price_filtered_payload = price_filtered_response.json()
    assert price_filtered_payload
    for row in price_filtered_payload:
        assert 1.5 <= row["decimal_price"] <= 3.5

    diff_filtered_response = client.get(
        "/api/v1/odds/search",
        params={
            "bookmaker": "sportsbet",
            "scope": "player",
            "min_diff_last_10": -0.05,
            "max_diff_last_10": 0.2,
            "min_diff_2025": -0.2,
            "max_diff_2025": 0.2,
            "limit": 50,
        },
    )
    assert diff_filtered_response.status_code == 200
    diff_filtered_payload = diff_filtered_response.json()
    assert diff_filtered_payload
    for row in diff_filtered_payload:
        assert row["diff_last_10"] is not None
        assert row["diff_2025"] is not None
        assert -0.05 <= row["diff_last_10"] <= 0.2
        assert -0.2 <= row["diff_2025"] <= 0.2

    matchup_filtered_response = client.get(
        "/api/v1/odds/search",
        params={
            "bookmaker": "sportsbet",
            "scope": "player",
            "matchup_difficulty": "Bad",
            "limit": 50,
        },
    )
    assert matchup_filtered_response.status_code == 200
    matchup_filtered_payload = matchup_filtered_response.json()
    if matchup_filtered_payload:
        assert all(row["over_matchup_difficulty"] == "Bad" for row in matchup_filtered_payload)

    under_matchup_response = client.get(
        "/api/v1/odds/search",
        params={
            "scope": "player",
            "selection_type": "under",
            "matchup_difficulty": "Terrible",
            "limit": 50,
        },
    )
    assert under_matchup_response.status_code == 200
    under_matchup_payload = under_matchup_response.json()
    if under_matchup_payload:
        assert all(row["selection_type"] == "under" for row in under_matchup_payload)
        assert all(row["over_matchup_difficulty"] == "Terrible" for row in under_matchup_payload)

    home_away_edge_response = client.get(
        "/api/v1/odds/search",
        params={"scope": "player", "favorable_home_away": True, "limit": 200},
    )
    assert home_away_edge_response.status_code == 200
    for row in home_away_edge_response.json():
        context_factor = 1 if row["player_home_away"] == "Home" else -1
        assert row["home_away_diff"] * context_factor > 0

    win_loss_edge_response = client.get(
        "/api/v1/odds/search",
        params={"scope": "player", "favorable_win_loss": True, "limit": 200},
    )
    assert win_loss_edge_response.status_code == 200
    for row in win_loss_edge_response.json():
        result_factor = 1 if row["player_team_line"] > 0 else -1
        assert row["win_loss_diff"] * result_factor > 0

    best_only_response = client.get(
        "/api/v1/odds/search",
        params={
            "bookmaker": "sportsbet",
            "scope": "player",
            "best_only": True,
            "limit": 50,
        },
    )
    assert best_only_response.status_code == 200
    best_only_payload = best_only_response.json()
    if best_only_payload:
        assert all(row["is_best_price"] is True for row in best_only_payload)

    cgm_candidate_response = client.get(
        "/api/v1/odds/search",
        params={
            "bookmaker": "sportsbet",
            "scope": "player",
            "limit": 200,
        },
    )
    assert cgm_candidate_response.status_code == 200
    cgm_candidate_rows = [
        row for row in cgm_candidate_response.json()
        if row["player"] is not None
    ]
    unique_event_rows = []
    seen_event_ids = set()
    for row in cgm_candidate_rows:
        event_id = row["event_id"]
        if event_id in seen_event_ids:
            continue
        seen_event_ids.add(event_id)
        unique_event_rows.append(row)
        if len(unique_event_rows) == 2:
            break
    assert len(unique_event_rows) == 2

    cgm_response = client.post(
        "/api/v1/pricing/cgm",
        json={"selection_ids": [row["selection_id"] for row in unique_event_rows]},
    )
    assert cgm_response.status_code == 200
    cgm_payload = cgm_response.json()
    assert cgm_payload["selection_count"] == 2
    assert cgm_payload["results"]
    assert cgm_payload["results"][0]["quoted_price"] > 0

    same_event_rows = []
    first_event_id = cgm_candidate_rows[0]["event_id"]
    for row in cgm_candidate_rows:
        if row["event_id"] == first_event_id:
            same_event_rows.append(row)
        if len(same_event_rows) == 2:
            break
    if len(same_event_rows) == 2:
        same_event_cgm_response = client.post(
            "/api/v1/pricing/cgm",
            json={"selection_ids": [row["selection_id"] for row in same_event_rows]},
        )
        assert same_event_cgm_response.status_code == 422
        assert same_event_cgm_response.json()["error"]["code"] == "duplicate_game_legs"

    stat_players_response = client.get("/api/v1/players/stats/search", params={"q": "English"})
    assert stat_players_response.status_code == 200
    stat_players_payload = stat_players_response.json()
    assert stat_players_payload
    filtered_out_response = client.get("/api/v1/players/stats/search", params={"q": "English", "stat": "disposals", "seasons": "2999"})
    assert filtered_out_response.status_code == 200
    assert filtered_out_response.json() == []
    assert all(
        " over " not in player["full_name"].lower()
        and " under " not in player["full_name"].lower()
        and "1st-" not in player["full_name"].lower()
        and "2nd-" not in player["full_name"].lower()
        and "3rd-" not in player["full_name"].lower()
        and "4th-" not in player["full_name"].lower()
        for player in stat_players_payload
    )
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


def test_odds_search_accepts_multiple_event_ids(client) -> None:
    events_response = client.get("/api/v1/events", params={"bookmaker": "sportsbet", "limit": 10})
    assert events_response.status_code == 200
    events = events_response.json()
    assert events

    event_ids = [event["id"] for event in events[:2]]

    # Single event_id keeps working (backwards compatible).
    single_response = client.get(
        "/api/v1/odds/search",
        params={"bookmaker": "sportsbet", "event_id": event_ids[0], "limit": 200},
    )
    assert single_response.status_code == 200
    single_rows = single_response.json()
    assert all(row["event_id"] == event_ids[0] for row in single_rows)

    # Repeated event_id params restrict to exactly that set of events.
    multi_response = client.get(
        "/api/v1/odds/search",
        params={"bookmaker": "sportsbet", "event_id": event_ids, "limit": 500},
    )
    assert multi_response.status_code == 200
    multi_rows = multi_response.json()
    assert multi_rows
    assert all(row["event_id"] in event_ids for row in multi_rows)
    if len(event_ids) > 1 and single_rows:
        returned_event_ids = {row["event_id"] for row in multi_rows}
        assert returned_event_ids.issubset(set(event_ids))


def test_player_stat_filters_narrow_venues(client) -> None:
    stat_players_response = client.get("/api/v1/players/stats/search", params={"limit": 1})
    assert stat_players_response.status_code == 200
    players = stat_players_response.json()
    assert players
    player_id = players[0]["id"]

    full_response = client.get(f"/api/v1/players/{player_id}/stats/filters")
    assert full_response.status_code == 200
    full_payload = full_response.json()
    all_venues = full_payload["venues"]
    assert all_venues

    # Narrowing by the most recent season may only ever shrink the venue list,
    # and must agree exactly with the venues present in the history endpoint's
    # response for the same filters.
    season = full_payload["seasons"][0]
    narrowed_response = client.get(
        f"/api/v1/players/{player_id}/stats/filters",
        params={"seasons": [season]},
    )
    assert narrowed_response.status_code == 200
    narrowed_venues = narrowed_response.json()["venues"]
    assert set(narrowed_venues).issubset(set(all_venues))

    history_response = client.get(
        f"/api/v1/players/{player_id}/stats/history",
        params={"stat": "disposals", "seasons": [season]},
    )
    assert history_response.status_code == 200
    history_venues = {row["venue"] for row in history_response.json() if row["venue"] is not None}
    assert set(narrowed_venues) == history_venues
