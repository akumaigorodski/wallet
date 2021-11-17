package immortan.utils

import java.io.ByteArrayInputStream
import java.nio.ByteOrder

import fr.acinq.bitcoin.Protocol
import scodec.bits.ByteVector


object Haiku {
  val adjs = List("autumn", "hidden", "bitter", "misty", "silent", "online",
    "empty", "dry", "dark", "summer", "icy", "delicate", "quiet", "white", "cool",
    "spring", "winter", "patient", "twilight", "dawn", "crimson", "wispy",
    "weathered", "blue", "billowing", "broken", "cold", "damp", "falling",
    "frosty", "green", "long", "late", "lingering", "bold", "little", "morning",
    "muddy", "old", "red", "rough", "still", "small", "sparkling",
    "shy", "wandering", "withered", "wild", "black", "holy", "solitary",
    "fragrant", "aged", "snowy", "proud", "floral", "restless", "divine",
    "polished", "purple", "lively", "nameless", "puffy", "fluffy",
    "calm", "young", "golden", "avenging", "ancestral", "ancient", "argent",
    "reckless", "daunting", "short", "rising", "strong", "timber", "tumbling",
    "silver", "dusty", "celestial", "cosmic", "crescent", "double", "far", "half",
    "inner", "milky", "northern", "southern", "eastern", "western", "outer",
    "terrestrial", "huge", "deep", "epic", "titanic", "mighty", "powerful")

  val nouns = List("waterfall", "river", "breeze", "moon", "rain", "pony",
    "wind", "sea", "morning", "snow", "lake", "sunset", "shadow", "leaf",
    "dawn", "glitter", "forest", "hill", "cloud", "meadow", "glade",
    "bird", "brook", "butterfly", "bush", "dew", "dust", "field",
    "flower", "firefly", "feather", "grass", "haze", "mountain", "night",
    "darkness", "snowflake", "silence", "sound", "sky", "shape", "surf",
    "thunder", "violet", "wildflower", "wave", "water", "resonance",
    "sun", "wood", "dream", "cherry", "tree", "fog", "frost", "voice", "paper",
    "frog", "smoke", "star", "sierra", "castle", "fortress", "tiger", "day",
    "sequoia", "cedar", "wrath", "blessing", "spirit", "nova", "storm", "burst",
    "protector", "drake", "dragon", "knight", "fire", "king", "jungle", "queen",
    "giant", "elemental", "throne", "game", "weed", "stone", "apogee", "bang",
    "cluster", "corona", "cosmos", "equinox", "horizon", "light",
    "solstice", "spectrum", "universe", "magnitude", "parallax")

  require(adjs.size == nouns.size)

  val divisor: Long = Long.MaxValue / adjs.size

  def name(data: ByteVector): String = {
    val stream = new ByteArrayInputStream(data.toArray)
    val adj: Long = math.abs(Protocol.uint64(stream, ByteOrder.BIG_ENDIAN) / divisor)
    val noun: Long = math.abs(Protocol.uint64(stream, ByteOrder.BIG_ENDIAN) / divisor)

    val adjString = adjs(adj.toInt)
    val nounString = nouns(noun.toInt)
    s"$adjString $nounString"
  }
}