# cashflows
module CashFlows

export CashFlow, accrual_period, accrual_days

using DataFrames
using DataArrays
using Calendar
using Ito.Time

type CashFlow
  cash_flow_df::DataFrame
end

function CashFlow(currency::Union{Vector{ASCIIString}, DataArray{ASCIIString}}, dates::Union{Vector{CalendarTime}, DataArray{CalendarTime}}, amounts::Union{Vector{Float64}, DataArray{Float64}})
  n1 = length(currency); n2 = length(dates); n3 = length(amounts)
  msg = "The constructor's arguments must have the same length"
  n1 == n2 == n3 || throw(ArgumentError(msg))
  CashFlow(DataFrame(currency = currency, dates = dates, amounts = amounts))
end

# main cashflow methods
start_date(cf::CashFlow) = sort(cf.cash_flow_df, cols = :dates)[:dates][1]
maturity_date(cf::CashFlow) = sort(cf:cash_flow_df, cols = :dates, rev = true)[:dates][1]
is_expired(cf::CashFlow, settle_date::CalendarTime) = maturity_date(cf) < settle_date

previous_cash_flow(cf::CashFlow, settle_date::CalendarTime) = cf.cash_flow_df[cf.cash_flow_df[:dates] .< settle_date, :]
next_cash_flow(cf::CashFlow, settle_date::CalendarTime) = cf.cash_flow_df[cf.cashflow[:dates] .> settle_date, :]

function previous_cash_flow_date(cf::CashFlow, settle_date::CalendarTime)
  prev_df = sort(previous_cash_flow(cf, settle_date), cols = :dates, rev = true)

  if size(prev_df, 1) == 0
    return
  else
    return prev_df[:dates][1]
  end
end

function next_cash_flow_date(cf::CashFlow, settle_date::CalendarTime)
  next_df = sort(next_cash_flow(cf, settle_date), cols :dates)

  if size(next_df, 1) == 0
    return
  else
    return next_df[:dates][1]
  end
end

function previous_cash_flow_amount(cf::CashFlow, settle_date::CalendarTime)
  prev_df = sort(previous_cash_flow(cf, settle_date), cols = :dates, rev = true)

  if size(prev_df, 1) == 0
    return
  else
    return prev_df[:amounts][1]
  end
end

function next_cash_flow_amount(cf::CashFlow, settle_date::CalendarTime)
  next_df = sort(next_cash_flow(cf, settle_date), cols :dates)

  if size(next_df, 1) == 0
    return
  else
    return next_df[:amounts][1]
  end
end

function accrual_period(cf::CashFlow, dc::DayCount, settle_date::CalendarTime)
  # first, get the last payment date
  last_payment = previous_cash_flow_date(cf, settle_date)

  if last_payment == nothing
    return 0.0
  else
    # now get the period as a year fraction
    return yearfraction(dc, last_payment, settle_date)
  end
end

function accrual_days(cf::CashFlow, dc::DayCount, settle_date::CalendarTime)
  # first, get the last payment date
  last_payment = previous_cash_flow_date(cf, settle_date)

  if last_payment == nothing
    return 0.0
  else
    # now get the number of days
    return daycount(dc, last_payment, settle_date)
  end
end

end # end module
