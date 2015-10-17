module TermStructure

using Ito.Time
using Calendar
using Dierckx

export YieldTermStructure, VolatilityTermStructure, FlatYieldTermStructure,
		compound_factor, discount_factor, implied_rate, discount, forward_rate,
		reference_date, zero_rate, par_rate,CompoundingType, CurveMethod, ZeroYieldTermStructure,
		ERF, ER, discountfactor, ERF_to_rate, discountfactor_to_rate


const NoFrequency       = -1
const Once			 	= 0
const Annual			= 1
const Semiannual		= 2
const EveryFourthMonth  = 3
const Quarterly		 	= 4
const Bimonthly		 	= 6
const Monthly			= 12
const EveryFourthWeek  	= 13
const Biweekly		 	= 26
const Weekly			= 52
const Daily			 	= 365
const OtherFrequency   	= 999

# Compounding Types
abstract CompoundingType
type ContinuousCompounding <: CompoundingType end   # exp(r*t)
type SimpleCompounding <: CompoundingType end       # (1+r*t)
type ExponentialCompounding <: CompoundingType end  # (1+r)^t

abstract CurveMethod
# abstract Parametric <: CurveMethod
abstract Interpolation <: CurveMethod

abstract DiscountFactorInterpolation <: Interpolation
abstract RateInterpolation <: Interpolation

type CubicSplineOnRates <: RateInterpolation end
type CubicSplineOnDiscountFactors <: DiscountFactorInterpolation end
type FlatForward <: DiscountFactorInterpolation end
type Linear <: RateInterpolation end
# type NelsonSiegel <: Parametric end
# type Svensson <: Parametric end
type StepFunction <: RateInterpolation end

#General Interest Rate functionality

compound_factor(r::Real, compounding::Symbol, freq::Symbol,  t::Real) = compound_factor(r, compounding, eval(freq), t)
compound_factor(r::Real, compounding::Symbol,  t::Real) = compound_factor(r, compounding, NoFrequency, t)
compound_factor(r::Real, compounding::Symbol, dc::DayCount, dates::CalendarTime...) = compound_factor(r, compounding, NoFrequency, dates...)
compound_factor(r::Real, compounding::Symbol, freq::Symbol, dc::DayCount, dates::CalendarTime...) = compound_factor(r, compounding, eval(freq), dates...)
compound_factor(r::Real, compounding::Symbol, freq::Integer, dc::DayCount, dates::CalendarTime...) = compound_factor(r,  compounding, freq, yearfraction(dc, dates...))

function compound_factor(r::Float64, compounding::Symbol, freq::Integer, t::Float64 )
	if compounding == :Simple
		return 1 + r * t
	elseif compounding == :Compounded
		@assert(freq != NoFrequency)
		return (1 + r / freq) ^ (freq * t)
	elseif compounding == :Continuous
		return exp(r * t)
	elseif compounding == :SimpleThenCompounded
		@assert(freq != NoFrequency)
		if t < (1 / freq)
            return 1 + r * t
        else
            return (1 + r / freq) ^ (freq * t)
        end
	else
		error("Unknown compounding")
	end
end

discount_factor(x...) = 1/compound_factor(x...)

implied_rate(c::Real, compounding::Symbol, freq::Symbol,  t::Real) = implied_rate(c,compounding, eval(freq), t)
implied_rate(c::Real, compounding::Symbol, freq::Symbol, dc::DayCount, dates::CalendarTime...) = implied_rate(c,compounding, eval(freq), dates...)
implied_rate(c::Real, compounding::Symbol, freq::Integer, dc::DayCount, dates::CalendarTime...) = implied_rate(c, compounding, freq, yearfraction(dc, dates...))

function implied_rate(c::Real, compounding::Symbol, freq::Integer, t::Real)
	@assert c>0 && t>0
	if compounding == :Simple
		return (c-1) / t
	elseif compounding == :Compounded
		return (c ^ (1/(f*t)) - 1) *f
	elseif compounding == :Continuous
		return log(c)/t
	elseif compounding == :SimpleThenCompounded
		if t < (1 / freq)
            return (c-1) / t
        else
            return (c ^ (1/(f*t)) - 1) *f
        end
	else
		error("Unknown compounding")
	end
end



#abstract TermStructure

abstract VolatilityTermStructure
abstract YieldTermStructure

discount(ts::YieldTermStructure, d::CalendarTime) = discount(ts, yearfraction(reference_date(ts), d))

#Overload this method for each concrete YieldTermStructure
discount(ts::YieldTermStructure, t::Real) = error("Must be implemented by concrete Term Structure")

#Conventional implementation. Usually re-implemented by concrete term structures
reference_date(ts::YieldTermStructure) = ts.reference_date

forward_rate(ts::YieldTermStructure, compounding::Symbol, freq::Integer, d1::CalendarTime, d2::CalendarTime) = forward_rate(ts, compounding, freq, d1, d2)
function forward_rate(ts::YieldTermStructure, compounding::Symbol, freq::Integer, d1::CalendarTime, d2::CalendarTime )
	if d1==d2
		t1 = yearfraction(reference_date(ts), d1)
		t2 = t1+.0001
		c=discount(ts, t1) / discount(ts, t2)
		return implied_rate(c, compound, freq, delta)
	elseif d1<d2
		return implied_rate(discount(ts, d1)/discount(ts, d2), ts.dc, d1, d2)
	else
		error("Forward start date must be before forward end dates")
	end
end

function forward_rate(ts::YieldTermStructure, compounding::Symbol, freq::Symbol, t1::Float64, t2::Float64 )
	if (t2==t1)
		t2=t1+.0001
	end

	compound = discount(ts, t1) / discount(ts, t2)
	return implied_rate(discount(ts, t1) / discount(ts, t2), ts.compounding, ts.freq, t1)
end

zero_rate(ts::YieldTermStructure, compounding::Symbol, freq::Integer, d1::CalendarTime) = zero_rate(ts, compounding, freq, yearfraction(ts.dc, reference_date(ts), d1))
function zero_rate(ts::YieldTermStructure, compounding::Symbol, freq::Integer, t::Real)
	if (t == 0)
		c = 1/discount(ts, .0001)
		return implied_rate(c, compounding, freq, .0001)
	else
		c = 1/discount(ts, t)
		return implied_rate(c, compounding, freq, t)
	end
end

par_rate(ts::YieldTermStructure, compounding, freq, dates::AbstractVector{CalendarTime}) =
	par_rate(ts, compounding, freq, [yearfraction(ts.dc, reference_date(ts), dt) for dt in dates])
function par_rate(ts::YieldTermStructure, compounding, freq, tm::AbstractVector)
	s = sum([discount(ts, t) for t in tm])
	r=discount(ts, tm[1])
end

type ConstantVolatilityTermStructure

end

type FlatYieldTermStructure <: YieldTermStructure
	dc::DayCount
	rate::Float64
	compounding::Symbol
	freq::Symbol
	reference_date::CalendarTime

	FlatYieldTermStructure(dc::DayCount, rate::Float64, compounding::Symbol, freq::Symbol, reference_date::CalendarTime) =
			new(dc, rate, compounding, freq, reference_date)
end

type BlackVolTermStructure <: VolatilityTermStructure
	dc::DayCount
	reference_date::CalendarTime
	vol::Float64
end

FlatYieldTermStructure(dc::DayCount, rate::Real) = FlatYieldTermStructure(dc, rate, :Continuous, :NoFrequency, today())
discount(ts::FlatYieldTermStructure, t::Float64) = discount_factor(ts.rate, ts.compounding, ts.freq, t)

type ZeroYieldTermStructure <: YieldTermStructure
	ref_date::CalendarTime
	day_jumps::Vector{Int64}
	rate_jumps::Vector{Float64}
	dc::DayCount
	calendar::BusinessCalendar
	interpolation::CurveMethod
	compounding::CompoundingType
	freq::Int64

	function ZeroYieldTermStructure(ref_date::CalendarTime, spot_dates::Vector{CalendarTime}, rate_jumps::Vector{Float64}, dc::DayCount, calendar::BusinessCalendar, interpolation::CurveMethod,
																	compounding::CompoundingType, freq::Symbol)
		# build the dtm Vector
		vec_size = length(spot_dates)
		dtm_vec = zeros(Int64, vec_size)
		for i = 1:vec_size
			dtm_vec[i] = Int(spot_dates[i] - ref_date)
		end

		new(ref_date, dtm_vec, rate_jumps, dc, calendar, interpolation, compounding, eval(freq))
	end
end


######### ZERO TERM YIELD STRUCTURE METHODS ############

# Effective Rate Factor
_ERF(::ContinuousCompounding, r::Float64, t::Float64, ::Int64) = t == 0.0 ? 1.0 : exp(r*t)
_ERF(::SimpleCompounding, r::Float64, t::Float64, ::Int64) = t == 0.0 ? 1.0 : 1.0 + r*t
# _ERF(::ExponentialCompounding, r::Float64, t::Float64) = t == 0.0 ? 1.0 : (1.0+r)^t
_ERF(::ExponentialCompounding, r::Float64, t::Float64, freq::Int64) = t == 0.0 ? 1.0 : (1.0+(r / freq)) ^ (t * freq)
_ERF(ct::CompoundingType, dcc::DayCount, r::Float64, date_start::CalendarTime, date_end::CalendarTime, freq::Int64) = _ERF(ct, r, yearfraction(dcc, date_start, date_end), freq)
ERF(curve::ZeroYieldTermStructure, maturity::CalendarTime) = _ERF(curve.compounding, curve.dc, zero_rate(curve, maturity), curve.ref_date, maturity, curve.freq)
ERF(curve::ZeroYieldTermStructure, forward_date::CalendarTime, maturity::CalendarTime) = ERF(curve, maturity) / ERF(curve, forward_date)

_ERF_to_rate(::ContinuousCompounding, ERF::Float64, t::Float64, ::Int64) = log(ERF) / t
_ERF_to_rate(::SimpleCompounding, ERF::Float64, t::Float64, ::Int64) = (ERF-1.0) / t
_ERF_to_rate(::ExponentialCompounding, ERF::Float64, t::Float64, freq::Int64) = (ERF^(1.0/(t * freq)) - 1.0) * freq
ERF_to_rate(curve::ZeroYieldTermStructure, ERF::Float64, t::Float64) = _ERF_to_rate(curve.compounding, ERF, t, curve.freq)

discountfactor_to_rate(c::CompoundingType, _discountfactor_::Float64, t::Float64, freq::Int64) = _ERF_to_rate(c, 1.0 / _discountfactor_, t, freq)

function discountfactor_to_rate(c::CompoundingType, _discountfactor_vec_::Vector{Float64}, t_vec::Vector{Float64}, freq::Int64)
	l = length(_discountfactor_vec_)

	if l != length(t_vec)
		error("_discountfactor_vec_ and t_vec must have the same length. ($l != $(length(t_vec)))")
	end

	result = Array(Float64, l)
	for i in 1:l
		result[i] = discountfactor_to_rate(c, _discountfactor_vec_[i], t_vec[i], freq)
	end

	return result
end

# Effective Rate = [Effective Rate Factor] - 1
_ER(c::CompoundingType, r::Float64, t::Float64, freq::Int64) = _ERF(c, r, t, freq) - 1.0
_ER(ct::CompoundingType, dcc::DayCount, r::Float64, date_start::CalendarTime, date_end::CalendarTime, freq::Int64) = _ER(ct, r, yearfraction(dcc, date_start, date_end), freq)
ER(curve::ZeroYieldTermStructure, maturity::CalendarTime) = _ER(curve.compounding, curve.dc, zero_rate(curve, maturity), curve.ref_date, maturity, curve.freq)
ER(curve::ZeroYieldTermStructure, forward_date::CalendarTime, maturity::CalendarTime) =  ERF(curve, forward_date, maturity) - 1.0

# [Discount Factor] = 1 / [Effective Rate Factor]
_discountfactor(ct::CompoundingType, r::Float64, t::Float64, freq::Int64) = 1.0 / _ERF(ct, r, t, freq)
_discountfactor(ct::CompoundingType, dcc::DayCount, r::Float64, date_start::CalendarTime, date_end::CalendarTime, freq::Int64) = _discountfactor(ct, r, yearfraction(dcc, date_start, date_end), freq)
discountfactor(curve::ZeroYieldTermStructure, maturity::CalendarTime) = _discountfactor(curve.compounding, curve.dc, zero_rate(curve, maturity), curve.ref_date, maturity, curve.freq)

# Vector function for ERF and discountfactor functions
for fun in (:ERF, :discountfactor)
	@eval begin
		function ($fun)(curve::ZeroYieldTermStructure, maturity_vec::Vector{CalendarTime})
			l = length(maturity_vec)
			_zero_rate_vec_ = zero_rate(curve, maturity_vec)
			result = Array(Float64, l)
			for i = 1:l
				result[i] = $(symbol('_', fun))(curve.compounding, curve.dc, _zero_rate_vec_[i], curve.ref_date, maturity_vec[i], curve.freq)
			end
			return result
		end
	end
end

forward_rate(curve::ZeroYieldTermStructure, forward_date::CalendarTime, maturity::CalendarTime) = ERF_to_rate(curve, ERF(curve, forward_date, maturity), yearfraction(curve.dc, forward_date, maturity))

function days_to_maturity(curve::ZeroYieldTermStructure, maturity::CalendarTime)
	const d = daycount(curve.dc, curve.ref_date, maturity)
	if d < 0
		error("Maturity date $(maturity) should be greater than curve observation date $(curve.dt_observation)")
	end
	return d
end

# Let's use the curve's method to multiple-dispatch. Ugly methods _zero_rate are not exported.
zero_rate(curve::ZeroYieldTermStructure, maturity::CalendarTime) = _zero_rate(curve.interpolation, curve, maturity)
zero_rate(curve::ZeroYieldTermStructure, maturity_vec::Vector{CalendarTime}) = _zero_rate(curve.interpolation, curve, maturity_vec)

# Curve methods implementation

_zero_rate{METHOD<:RateInterpolation}(method::METHOD, curve::ZeroYieldTermStructure, maturity::CalendarTime) = _zero_rate(method, curve.day_jumps, curve.rate_jumps, days_to_maturity(curve, maturity))

function _zero_rate(::Linear, x::Vector{Int}, y::Vector{Float64}, x_out::Int)
	# If this curve has only 1 vertice, this will be a flat curve
	if length(x) == 1
		return y[1]
	end

	# index_a, index_b = _interpolationpoints(x, x_out)
	# return _linearinterp(x[index_a], y[index_a], x[index_b], y[index_b], x_out)
	return Spline1D(x, y; k = 1, bc = "nearest")(x_out)
end

end #Module
