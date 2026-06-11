package com.jamesbrown.aflmobile.data.network

import com.jamesbrown.aflmobile.BuildConfig
import com.jamesbrown.aflmobile.data.settings.AppSettingsStore
import com.jamesbrown.aflmobile.model.ApiErrorEnvelope
import com.jamesbrown.aflmobile.model.BookmakerSummary
import com.jamesbrown.aflmobile.model.CgmCompareRequestPayload
import com.jamesbrown.aflmobile.model.CgmCompareResponse
import com.jamesbrown.aflmobile.model.DataStatusResponse
import com.jamesbrown.aflmobile.model.EventSummary
import com.jamesbrown.aflmobile.model.HealthResponse
import com.jamesbrown.aflmobile.model.MarketSummary
import com.jamesbrown.aflmobile.model.OddsQuery
import com.jamesbrown.aflmobile.model.OddsSearchResult
import com.jamesbrown.aflmobile.model.PlayerGameLogEntry
import com.jamesbrown.aflmobile.model.PlayerStatFilterOptions
import com.jamesbrown.aflmobile.model.PlayerStatSummary
import com.jamesbrown.aflmobile.model.PlayerSummary
import com.jamesbrown.aflmobile.model.PropSearchResult
import com.jamesbrown.aflmobile.model.SelectionSummary
import com.jamesbrown.aflmobile.model.SgmCompareRequestPayload
import com.jamesbrown.aflmobile.model.SgmCompareResponse
import java.time.Duration
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import kotlinx.serialization.SerializationException
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import okhttp3.HttpUrl.Companion.toHttpUrl
import okhttp3.MediaType.Companion.toMediaType
import okhttp3.OkHttpClient
import okhttp3.Request
import okhttp3.RequestBody.Companion.toRequestBody
import okhttp3.logging.HttpLoggingInterceptor


class BackendApiClient(
    private val settingsStore: AppSettingsStore,
    private val json: Json = Json { ignoreUnknownKeys = true; explicitNulls = false },
) {
    private val client: OkHttpClient = OkHttpClient.Builder()
        .connectTimeout(Duration.ofSeconds(10))
        .readTimeout(Duration.ofSeconds(30))
        .callTimeout(Duration.ofSeconds(45))
        .apply {
            if (BuildConfig.DEBUG) {
                addInterceptor(
                    HttpLoggingInterceptor().apply {
                        level = HttpLoggingInterceptor.Level.BASIC
                    },
                )
            }
        }
        .build()

    suspend fun getHealth(): HealthResponse = get("health")

    suspend fun getDataStatus(): DataStatusResponse = get("data/status")

    suspend fun getBookmakers(): List<BookmakerSummary> = get("bookmakers")

    suspend fun getEvents(
        bookmaker: String? = null,
        query: String? = null,
        limit: Int = 50,
    ): List<EventSummary> = get(
        path = "events",
        query = buildList {
            add("limit" to limit.toString())
            bookmaker?.takeIf { it.isNotBlank() }?.let { add("bookmaker" to it) }
            query?.takeIf { it.isNotBlank() }?.let { add("q" to it) }
        },
    )

    suspend fun getEvent(eventId: Int): EventSummary = get("events/$eventId")

    suspend fun getMarkets(
        eventId: Int,
        bookmaker: String,
        playerQuery: String? = null,
        limit: Int = 100,
    ): List<MarketSummary> = get(
        path = "events/$eventId/markets",
        query = buildList {
            add("bookmaker" to bookmaker)
            add("limit" to limit.toString())
            playerQuery?.takeIf { it.isNotBlank() }?.let { add("player_q" to it) }
        },
    )

    suspend fun getSelections(
        marketId: Int,
        bookmaker: String,
    ): List<SelectionSummary> = get(
        path = "markets/$marketId/selections",
        query = listOf("bookmaker" to bookmaker),
    )

    suspend fun searchPlayers(
        query: String,
        limit: Int = 50,
    ): List<PlayerSummary> = get(
        path = "players/search",
        query = buildList {
            add("q" to query)
            add("limit" to limit.toString())
        },
    )

    suspend fun searchStatPlayers(
        query: String,
        limit: Int = 50,
    ): List<PlayerSummary> = get(
        path = "players/stats/search",
        query = buildList {
            add("q" to query)
            add("limit" to limit.toString())
        },
    )

    suspend fun getPlayerStatFilters(playerId: Int): PlayerStatFilterOptions =
        get("players/$playerId/stats/filters")

    /**
     * Filter options narrowed by the current game-log filters. The backend
     * computes the venue list with the same pipeline as the history endpoint,
     * so the app never re-implements that filtering locally.
     */
    suspend fun getPlayerStatFiltersNarrowed(
        playerId: Int,
        seasons: List<String>,
        oppositions: List<String>,
        weatherCategories: List<String>,
        homeAway: List<String>,
        marginMin: Int,
        marginMax: Int,
        lastGames: Int?,
        minutesMinimum: Double,
    ): PlayerStatFilterOptions = get(
        path = "players/$playerId/stats/filters",
        query = buildList {
            seasons.forEach { add("seasons" to it) }
            oppositions.forEach { add("oppositions" to it) }
            weatherCategories.forEach { add("weather_categories" to it) }
            homeAway.forEach { add("home_away" to it) }
            add("margin_min" to marginMin.toString())
            add("margin_max" to marginMax.toString())
            add("minutes_minimum" to minutesMinimum.toString())
            lastGames?.let { add("last_games" to it.toString()) }
        },
    )

    suspend fun getPlayerStatHistory(
        playerId: Int,
        stat: String,
        seasons: List<String>,
        oppositions: List<String>,
        venues: List<String>,
        weatherCategories: List<String>,
        homeAway: List<String>,
        marginMin: Int,
        marginMax: Int,
        lastGames: Int? = null,
        minutesMinimum: Double = 0.0,
        lineMode: String? = null,
        referenceLine: Double? = null,
        lowerBound: Double? = null,
        upperBound: Double? = null,
    ): List<PlayerGameLogEntry> = get(
        path = "players/$playerId/stats/history",
        query = buildList {
            add("stat" to stat)
            seasons.forEach { add("seasons" to it) }
            oppositions.forEach { add("oppositions" to it) }
            venues.forEach { add("venues" to it) }
            weatherCategories.forEach { add("weather_categories" to it) }
            homeAway.forEach { add("home_away" to it) }
            add("margin_min" to marginMin.toString())
            add("margin_max" to marginMax.toString())
            add("minutes_minimum" to minutesMinimum.toString())
            lastGames?.let { add("last_games" to it.toString()) }
            lineMode?.let { add("line_mode" to it) }
            referenceLine?.let { add("reference_line" to it.toString()) }
            lowerBound?.let { add("lower_bound" to it.toString()) }
            upperBound?.let { add("upper_bound" to it.toString()) }
        },
    )

    suspend fun getPlayerStatSummary(
        playerId: Int,
        stat: String,
        lineMode: String,
        referenceLine: Double? = null,
        lowerBound: Double? = null,
        upperBound: Double? = null,
        seasons: List<String>,
        oppositions: List<String>,
        venues: List<String>,
        weatherCategories: List<String>,
        homeAway: List<String>,
        marginMin: Int,
        marginMax: Int,
        lastGames: Int? = null,
        minutesMinimum: Double = 0.0,
    ): PlayerStatSummary = get(
        path = "players/$playerId/stats/summary",
        query = buildList {
            add("stat" to stat)
            add("line_mode" to lineMode)
            seasons.forEach { add("seasons" to it) }
            oppositions.forEach { add("oppositions" to it) }
            venues.forEach { add("venues" to it) }
            weatherCategories.forEach { add("weather_categories" to it) }
            homeAway.forEach { add("home_away" to it) }
            add("margin_min" to marginMin.toString())
            add("margin_max" to marginMax.toString())
            add("minutes_minimum" to minutesMinimum.toString())
            lastGames?.let { add("last_games" to it.toString()) }
            referenceLine?.let { add("reference_line" to it.toString()) }
            lowerBound?.let { add("lower_bound" to it.toString()) }
            upperBound?.let { add("upper_bound" to it.toString()) }
        },
    )

    suspend fun searchOdds(oddsQuery: OddsQuery): List<OddsSearchResult> = get(
        path = "odds/search",
        query = buildList {
            add("limit" to oddsQuery.limit.toString())
            add("offset" to oddsQuery.offset.toString())
            add("scope" to oddsQuery.scope)
            oddsQuery.bookmakers.forEach { bookmaker ->
                if (bookmaker.isNotBlank()) {
                    add("bookmaker" to bookmaker)
                }
            }
            oddsQuery.marketType?.takeIf { it.isNotBlank() }?.let { add("market_type" to it) }
            oddsQuery.eventIds.forEach { add("event_id" to it.toString()) }
            oddsQuery.includePlayerIds.forEach { add("include_player_id" to it.toString()) }
            oddsQuery.excludePlayerIds.forEach { add("exclude_player_id" to it.toString()) }
            add("sort_by" to oddsQuery.sortBy)
            add("sort_dir" to oddsQuery.sortDirection)
            oddsQuery.selectionType?.takeIf { it.isNotBlank() }?.let { add("selection_type" to it) }
            oddsQuery.matchupDifficulties.forEach { add("matchup_difficulty" to it) }
            oddsQuery.minPrice?.let { add("min_price" to it.toString()) }
            oddsQuery.maxPrice?.let { add("max_price" to it.toString()) }
            oddsQuery.minDiff2025?.let { add("min_diff_2025" to it.toString()) }
            oddsQuery.maxDiff2025?.let { add("max_diff_2025" to it.toString()) }
            oddsQuery.minDiffLast10?.let { add("min_diff_last_10" to it.toString()) }
            oddsQuery.maxDiffLast10?.let { add("max_diff_last_10" to it.toString()) }
            oddsQuery.minNextBestProbDiff?.let { add("min_next_best_prob_diff" to it.toString()) }
            oddsQuery.maxNextBestProbDiff?.let { add("max_next_best_prob_diff" to it.toString()) }
            if (oddsQuery.sgmOnly) add("sgm_only" to "true")
            if (oddsQuery.bestOnly) add("best_only" to "true")
        },
    )

    suspend fun searchProps(
        bookmaker: String,
        query: String? = null,
        limit: Int = 100,
    ): List<PropSearchResult> = get(
        path = "props/search",
        query = buildList {
            add("bookmaker" to bookmaker)
            add("limit" to limit.toString())
            query?.takeIf { it.isNotBlank() }?.let { add("q" to it) }
        },
    )

    suspend fun compareSgm(request: SgmCompareRequestPayload): SgmCompareResponse =
        post("pricing/sgm/compare", request)

    suspend fun compareCgm(request: CgmCompareRequestPayload): CgmCompareResponse =
        post("pricing/cgm", request)

    private suspend inline fun <reified T> get(
        path: String,
        query: List<Pair<String, String>> = emptyList(),
    ): T = executeRequest(
        method = "GET",
        path = path,
        query = query,
        body = null,
    )

    private suspend inline fun <reified P, reified T> post(
        path: String,
        payload: P,
    ): T = executeRequest(
        method = "POST",
        path = path,
        query = emptyList(),
        body = json.encodeToString(payload),
    )

    private suspend inline fun <reified T> executeRequest(
        method: String,
        path: String,
        query: List<Pair<String, String>>,
        body: String?,
    ): T = withContext(Dispatchers.IO) {
        val settings = settingsStore.current()
        val baseUrl = settings.apiBaseUrl.toHttpUrl()
        val urlBuilder = baseUrl.newBuilder().addPathSegments(path)
        query.forEach { (key, value) ->
            if (value.isNotBlank()) {
                urlBuilder.addQueryParameter(key, value)
            }
        }

        val requestBuilder = Request.Builder()
            .url(urlBuilder.build())
            .header("Accept", "application/json")

        if (settings.authToken.isNotBlank()) {
            requestBuilder.header("Authorization", "Bearer ${settings.authToken}")
        }

        if (method == "POST") {
            requestBuilder.post(
                (body ?: "").toRequestBody("application/json".toMediaType()),
            )
        } else {
            requestBuilder.get()
        }

        client.newCall(requestBuilder.build()).execute().use { response ->
            val responseBody = response.body.string()
            if (!response.isSuccessful) {
                val apiError = responseBody.toApiError(json)
                throw BackendApiException(
                    statusCode = response.code,
                    code = apiError?.error?.code,
                    message = apiError?.error?.message ?: "Backend request failed with ${response.code}.",
                )
            }

            try {
                json.decodeFromString<T>(responseBody)
            } catch (exception: SerializationException) {
                throw BackendApiException(
                    statusCode = response.code,
                    code = "decode_error",
                    message = "Failed to decode backend response.",
                    cause = exception,
                )
            }
        }
    }

    private fun String.toApiError(json: Json): ApiErrorEnvelope? = try {
        json.decodeFromString<ApiErrorEnvelope>(this)
    } catch (_: Exception) {
        null
    }
}

class BackendApiException(
    val statusCode: Int,
    val code: String? = null,
    override val message: String,
    override val cause: Throwable? = null,
) : Exception(message, cause)
