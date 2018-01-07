package fr.thomasdufour.rng

// MMIX by Donald Knuth
case class Rng(seed: Long) {
  def next: Rng =
    Rng(6364136223846793005L * seed + 1442695040888963407L)
}
