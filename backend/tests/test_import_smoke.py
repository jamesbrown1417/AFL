from __future__ import annotations

from app.db.duckdb import connection, fetch_value


def test_import_populates_core_tables(imported_settings) -> None:
    with connection(settings=imported_settings) as conn:
        event_count = fetch_value(conn, "SELECT COUNT(*) FROM events")
        market_count = fetch_value(conn, "SELECT COUNT(*) FROM markets")
        selection_count = fetch_value(conn, "SELECT COUNT(*) FROM selections")
        price_count = fetch_value(conn, "SELECT COUNT(*) FROM outcome_prices")
        metric_count = fetch_value(conn, "SELECT COUNT(*) FROM selection_metrics")
        player_game_log_count = fetch_value(conn, "SELECT COUNT(*) FROM player_game_logs")
        bookmaker_count = fetch_value(conn, "SELECT COUNT(*) FROM bookmakers")

    assert event_count and int(event_count) > 0
    assert market_count and int(market_count) > 0
    assert selection_count and int(selection_count) > 0
    assert price_count and int(price_count) > 0
    assert metric_count and int(metric_count) > 0
    assert player_game_log_count and int(player_game_log_count) > 0
    assert bookmaker_count and int(bookmaker_count) >= 5
