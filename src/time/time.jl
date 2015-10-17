module Time

import Calendar
using Calendar

import Base.string, Base.convert

export easterMonday, isWeekend, isWorkingDay, BusinessCalendar, WesternCalendar, USSettlementCalendar, USNYSECalendar, USGovernmentBondCalendar, USNERCCalendar, DayCount,
  Actual360, Actual365, Thirty360, BondThirty360, EuroBondThirty360, ItalianThirty360, ISMAActualActual, ISDAActualActual, AFBActualActual, daycount, yearfraction

include("business_calendars.jl")

include("day_count.jl")

# need convert
Base.convert(::Type{Int64}, t::Calendar.FixedCalendarDuration) = floor(Integer, t.millis / 86400e3)

end
