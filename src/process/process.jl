module Process

using Ito.Time
using Ito.TermStructure
using Calendar

#Stochastic processes
abstract StochasticProcess

# defined types
type GeometricBrownianMotion <: StochasticProcess
	start::Float64
  mu::Float64
  sigma::Float64
end

type GenericBlackScholesProcess <: StochasticProcess
	start::Float64
	dts::YieldTermStructure
	yts::YieldTermStructure
	vol::VolatilityTermStructure
end

# general functions

evolve(sp::StochasticProcess, t0::Float64, x0::Float64, dt::Float64, dw::Float64) =
					apply_change(sp, apply_change(sp, x0 , drift_discretize(sp, t0, x0, dt)) , diffusion_discretize(sp, t0,x0,dt) * dw)
apply_change(ps::StochasticProcess, x::Float64, dx::Float64) = x + dx

covariance(sp::StochasticProcess, t0::Float64, x0::Float64, dt::Float64) = covariance_discretize(sp, t0, x0, dt)

#Euler discretization functions
drift_discretize(sp::StochasticProcess, t0::Float64, x0::Float64, dt::Float64) = drift(sp, t0, x0) * dt

diffusion_discretize(sp::StochasticProcess, t0::Float64, x0::Float64, dt::Float64) = diffusion(sp, t0, x0) * sqrt(dt)

function covariance_discretize(sp::StochasticProcess, t0::Float64, x0::Float64, dt::Float64)
	s= diffusion(sp, t0, x0)
	( s * s' ) .* dt
end

# GeometricBrownianMotion functions

diffusion(p::GeometricBrownianMotion, t::Float64, x::Float64) = p.sigma * x
drift(p::GeometricBrownianMotion, t::Float64, x::Float64) = p.mu * x
x0(p::GeometricBrownianMotion) = p.start


# GenericBlackScholesProcess functions

apply_change(sp::GenericBlackScholesProcess, x, dx) = x + exp(dx)

diffusion(sp::GenericBlackScholesProcess, t, x) = vol(sp, t, x)

function vol(sp::GenericBlackScholesProcess, t::Float64, x::Float64)
	return sp.vol.vol
end

function drift(sp::GenericBlackScholesProcess, t, x)
	t1 = t+.0001
	r = forward_rate(sp.yts, :Continuous, :NoFrequency, t, t1)
	d = forward_rate(sp.dts, :Continuous, :NoFrequency, t, t1)
  sigma = diffusion(sp, t, x)
  return r - d - 0.5 * sigma * sigma
end

end #module
