from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class VenueCoordinate:
    venue_name: str
    latitude: float
    longitude: float
    display_name: str | None = None


VENUE_COORDINATES: dict[str, VenueCoordinate] = {
    "Adelaide Oval": VenueCoordinate("Adelaide Oval", -34.9155596, 138.5962772),
    "Barossa Park": VenueCoordinate("Barossa Park", -34.5940115, 138.8618667),
    "Corroboree Group Oval Manuka": VenueCoordinate(
        "Corroboree Group Oval Manuka",
        -35.3179533,
        149.1347561,
        display_name="Manuka Oval",
    ),
    "ENGIE Stadium": VenueCoordinate("ENGIE Stadium", -33.8431422, 151.0676444),
    "GMHBA Stadium": VenueCoordinate("GMHBA Stadium", -38.1581856, 144.3544373),
    "Gabba": VenueCoordinate("Gabba", -27.4858850, 153.0379847, display_name="Brisbane Cricket Ground"),
    "Hands Oval": VenueCoordinate("Hands Oval", -33.3462568, 115.6427584),
    "MCG": VenueCoordinate("MCG", -37.8199090, 144.9832250, display_name="Melbourne Cricket Ground"),
    "Marvel Stadium": VenueCoordinate("Marvel Stadium", -37.8165290, 144.9475170),
    "Ninja Stadium": VenueCoordinate("Ninja Stadium", -42.8772971, 147.3737355, display_name="Bellerive Oval"),
    "Norwood Oval": VenueCoordinate("Norwood Oval", -34.9198188, 138.6304715),
    "Optus Stadium": VenueCoordinate("Optus Stadium", -31.9510448, 115.8890756),
    "People First Stadium": VenueCoordinate("People First Stadium", -28.0073025, 153.3674428),
    "SCG": VenueCoordinate("SCG", -33.8914241, 151.2247484, display_name="Sydney Cricket Ground"),
    "TIO Stadium": VenueCoordinate("TIO Stadium", -12.3991751, 130.8872850),
    "TIO Traeger Park": VenueCoordinate("TIO Traeger Park", -23.7090949, 133.8753753),
    "UTAS Stadium": VenueCoordinate("UTAS Stadium", -41.4259627, 147.1388579),
}
