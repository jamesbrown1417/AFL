package com.jamesbrown.aflmobile

import com.jamesbrown.aflmobile.ui.common.aflTeamCode
import com.jamesbrown.aflmobile.ui.common.shortAflMatchLabel
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Test


class FormattersTest {
    @Test
    fun aflTeamCode_resolvesAmbiguousNames() {
        assertEquals("PTA", aflTeamCode("Port Adelaide Power"))
        assertEquals("ADE", aflTeamCode("Adelaide Crows"))
        assertEquals("NTH", aflTeamCode("North Melbourne"))
        assertEquals("MEL", aflTeamCode("Melbourne"))
        assertEquals("GWS", aflTeamCode("Greater Western Sydney Giants"))
        assertEquals("SYD", aflTeamCode("Sydney Swans"))
        assertEquals("WCE", aflTeamCode("West Coast Eagles"))
        assertEquals("WBD", aflTeamCode("Western Bulldogs"))
    }

    @Test
    fun aflTeamCode_returnsNullForUnknownTeams() {
        assertNull(aflTeamCode("Auckland Warriors"))
    }

    @Test
    fun shortAflMatchLabel_shortensKnownMatchups() {
        assertEquals("COL v CAR", shortAflMatchLabel("Collingwood vs Carlton"))
        assertEquals("GEE v ESS", shortAflMatchLabel("Geelong v Essendon"))
    }

    @Test
    fun shortAflMatchLabel_passesThroughUnknownMatchups() {
        assertEquals("Someone vs Other", shortAflMatchLabel("Someone vs Other"))
    }
}
