module SpaceAge

type Planet = Earth | Mercury | Venus | Mars | Jupiter | Saturn | Uranus | Neptune

let baseOrbitalPeriod = 31557600L

let age planet seconds : float =
    let baseAge = float(seconds / baseOrbitalPeriod)
    let multiplier =
        match planet with
        | Earth -> 1.0
        | Mercury -> 0.2408467
        | Venus -> 0.61519726
        | Mars -> 1.8808158
        | Jupiter -> 11.862615
        | Saturn -> 29.447498
        | Uranus -> 84.016846
        | Neptune -> 164.79132
    System.Math.Round(baseAge / System.Math.Round(multiplier, 2), 2)

let res = age Earth 1000000000L