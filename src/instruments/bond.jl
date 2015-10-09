# Bond - included in the instruments module

type Bond <: Instrument
  settlementDays::Int64
  cal::BusinessCalendar
  dc::DayCount
  issue_date::CalendarTime
  frequency::Symbol
  coupon_rate::Float64
  coupons::CashFlow
  maturity_date::CalendarTime
  face_value::Float64
end

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

function accrued_amount(bond::Bond, settlement::CalendarTime)
  cur_notional = notional(bond, settlement)
  if cur_notional == 0.0
    return 0.0
  else
    accrual_days = CashFlows.accrual_days(bond.coupons, bond.dc, settlement)

    #TODO: make total period days calc more dynamic
    freq = eval(bond.frequency)
    tot_days_period = 360 / freq
    return ((bond.coupon_rate * bond.face_value) / freq) * (accrual_days / tot_days_period)
  end
end

function notional(bond::Bond, settlement::CalendarTime)
  if settlement > bond.maturity_date
    # past maturity
    return 0.0
  else
    return bond.face_value
  end
end
