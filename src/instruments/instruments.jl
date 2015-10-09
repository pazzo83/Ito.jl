module Instruments

import Base
using Ito.Time
using Calendar
using Ito.CashFlows

export VanillaPayoff, ForwardPayoff, VanillaOption, AmericanOption, Instrument, Bond, accrued_amount

abstract Payoff
abstract Instrument

include("bond.jl")

type VanillaPayoff <: Payoff
	typ
	strike
end

type ForwardPayoff <: Payoff
	typ
	strike
end

type StraddlePayoff <: Payoff
	typ
	strike
end

#ref(p::VanillaPayoff, s::Real) = typ == :call ? max(s - p.strike, 0) : max(p.strike - s, 0)
#ref(p::ForwardPayoff, s::Real) = typ == :long ? (s - p.strike) : (p.strike - s)
#ref(p::StraddlePayoff, s::Real) = typ == :long ? max(s - p.strike, 0) + max(p.strike - s, 0) : -max(s - p.strike, 0) - max(p.strike - s, 0)

type VanillaOption
	payoff
	excercise
end

type AmericanOption
	payoff
	excercise
end

end
