import it.unibo.geo.altitude.*
import kotlinx.serialization.*
import kotlinx.serialization.json.Json
import java.io.File
import java.net.URL

// Toronto coord = -79.639273,43.580253,-79.113219,43.855442
const val endpoint = "https://geogratis.gc.ca/services/elevation/cdem/altitude?"
const val storeIn = "src/main/resources/altitude.json"
const val precision = 100
@Serializable
data class AltitudeData(val altitude: Double, val vertex: Boolean, val geometry: Geometry)
@Serializable
data class Geometry(val type: String, val coordinates: List<Double>)
typealias BoundingBox = Pair<Double, Double>
fun BoundingBox.lat() = this.first
fun BoundingBox.lon() = this.second

val leftmost = BoundingBox(43.580253, -79.639273)
val rightmost = BoundingBox(43.855442, -79.113219)
val deltaLat = (rightmost.lat() - leftmost.lat()) / precision
val deltaLon = (rightmost.lon() - leftmost.lon()) / precision

// Script to download data from toronto altitude service
fun main() {
    println("Download Toronto File... ")
    val result = (0 until precision)
        .flatMap { lat -> (0 until precision).map { Pair(lat, it) } }
        .parallelStream()
        .map { BoundingBox(it.first * deltaLat + leftmost.lat(), it.second * deltaLon + leftmost.lon()) }
        .map { URL("${endpoint}lat=${it.lat()}&lon=${it.lon()}").readText() }
        .map { Json.decodeFromString<AltitudeData>(it) }
    File(storeIn).writeText(Json.encodeToString(result.toList()))
}