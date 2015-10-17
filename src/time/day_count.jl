#included within module Time



export  DayCount, Actual360, Actual365, Thirty360, BondThirty360, EuroBondThirty360, ItalianThirty360, ISMAActualActual, ISDAActualActual, AFBActualActual,
		daycount, yearfraction

abstract DayCount

type Actual360 <:DayCount ; end
type Actual365 <: DayCount ; end
abstract Thirty360 <:DayCount

type BondThirty360 <: Thirty360; end
type EuroBondThirty360 <: Thirty360; end
type ItalianThirty360 <: Thirty360; end

typealias USAThirty360 BondThirty360
typealias EuroThirty360 EuroBondThirty360

abstract ActualActual <:DayCount

type ISMAActualActual <: ActualActual; end
type ISDAActualActual <: ActualActual; end
type AFBActualActual <: ActualActual; end

type BusinessDay252 <: DayCount
	bc::BusinessCalendar
end



string(::Actual360) = "Actual/360"
string(::Actual365) = "Actual/365 Fixed"
string(::BondThirty360) = "Thirty/360 Bond/USA basis"
string(::EuroBondThirty360) = "Thirty/360 EuroBond basis"
string(::ItalianThirty360) = "Thirty/360 Italian basis"


string(::ISMAActualActual) = "Actual/Actual ISMA basis"
string(::ISDAActualActual) = "Actual/Actual ISDA basis"
string(::AFBActualActual) = "Actual/Actual AFB basis"


#Assumes both CalendarTime objects represent midnight in the same tz.
#The rounding takes care of summer time and leap second
#Will fail if they represent arbitrary times of day
daycount(c::DayCount,  d_start::CalendarTime,  d_end::CalendarTime) = round(Integer, (d_end-d_start).millis/86400e3)

function daycount(c::BondThirty360,  d_start::CalendarTime,  d_end::CalendarTime)
	dd1 = day(d_start)
     dd2 = day(d_end)
     mm1 = month(d_start)
     mm2 = month(d_end)
     yy1 = year(d_start)
     yy2 = year(d_end)

    if dd2 == 31 && dd1 < 30
    	dd2 = 1
    	mm2 = mm2+1
    end

    return 360*(yy2-yy1) + 30*(mm2-mm1-1) + max(0, 30-dd1) + min(30, dd2)
end

function daycount(c::EuroBondThirty360,  d_start::CalendarTime,  d_end::CalendarTime)
	dd1 = day(d_start)
    dd2 = day(d_end)
    mm1 = month(d_start)
    mm2 = month(d_end)
    yy1 = year(d_start)
    yy2 = year(d_end)

   	return 360*(yy2-yy1) + 30*(mm2-mm1-1) + max(0, 30-dd1) + min(30, dd2)
end

function daycount(c::ItalianThirty360,  d_start::CalendarTime,  d_end::CalendarTime)
	dd1 = day(d_start)
    dd2 = day(d_end)
    mm1 = month(d_start)
    mm2 = month(d_end)
    yy1 = year(d_start)
    yy2 = year(d_end)

    if (mm1 == 2 && dd1 > 27)
        dd1 = 30
    end
    if (mm2 == 2 && dd2 > 27)
        dd2 = 30
    end

    return 360*(yy2-yy1) + 30*(mm2-mm1-1) + max(0, 30-dd1) + min(30, dd2)
end

daycount(c::BusinessDay252, d_start::CalendarTime, d_end::CalendarTime) = businessDaysBetween(c.bc, d_start, d_end)

yearfraction(c::Union{Actual360,Thirty360},  d_start::CalendarTime,  d_end::CalendarTime) = daycount(c, d_start, d_end) / 360

yearfraction(c::Actual365,  d_start::CalendarTime,  d_end::CalendarTime) = daycount(c, d_start, d_end) / 365

function yearfraction(c::ISDAActualActual,  d_start::CalendarTime,  d_end::CalendarTime )

    if (d_start == d_end ); return 0.0; end

    if (d_start > d_end)
        return -yearFraction(d_end, d_start)
    end

    y1 = year(d_start)
    y2 = year(d_end)
    dib1 = isleapyear(d_start) ? 366.0 : 365.0
    dib2 = isleapyear(d_end)   ? 366.0 : 365.0

    sum = y2 - y1 - 1

    #Days from start to starting of following year
    sum += (dib1 - dayofyear(d_start) + 1) / dib1
    #Days from beginning of year to the endDate
    sum += (dayofyear(d_end) - 1) / dib2
    return sum
end

function yearfraction(c::AFBActualActual,  d_start::CalendarTime,  d_end::CalendarTime )
	if (d_start == d_end ); return 0.0; end

    if (d_start > d_end)
        return -yearFraction(d_end, d_start)
    end

    newD2 = d_end
	temp = d_end
	sum = 0.0
	while (temp>d_start)
	    temp = newD2+years(-1)
	    if (day(temp) == 28 && month(temp) == 2 && isleapyear(temp))
	        temp + days(1)
	    end
	    if (temp>=(d_start))
	        sum += 1.0
	        newD2 = temp
	    end
	end

	den = 365.0

	if isleapyear(newD2)
	    if (newD2 > ymd(year(newD2), February, 29) && d_start < ymd(year(newD2), February, 29) )
	        den += 1.0
	    end
	elseif (isleapyear(d_start))
	    if (newD2 > (ymd(year(d_start), February, 29)) && d_start < (ymd(year(d_start), February, 29)))
	        den += 1.0
	    end
	end
	return sum + daycount(c, d_start, newD2) / den
end

yearfraction(c::ISMAActualActual,  d_start::CalendarTime,  d_end::CalendarTime ) = yearfraction(c, d_start, d_end, d_start, d_end)

function yearfraction(c::ISMAActualActual,  d_start::CalendarTime,  d_end::CalendarTime,  ref_start::CalendarTime,  ref_end::CalendarTime)

 	if (d_start == d_end ); return 0.0; end

    if (d_start > d_end)
        return -yearFraction(d_end, d_start, ref_start, ref_end)
    end

    ref_period_start =  ref_start
    ref_period_end   = ref_end

    @assert ref_period_end > ref_period_start && ref_period_end > d_start

    # estimate roughly the length in months of a period
    mnths = ifloor(0.5 + 12 * daycount(c,  ref_period_start, ref_period_end) / 365)

    # for short periods, take the reference period as 1 year from d_start
    if (mnths == 0)
        ref_period_start = d_start
        ref_period_end = d_start + year(1)
        mnths = 12
    end

    period = mnths / 12.0

    if (d_end<=ref_period_end)
        # here ref_period_end is a future (notional?) payment date
        if (d_start>=ref_period_start)
            # here ref_period_start is the last (maybe notional)
            # payment date.
            # ref_period_start <= d_start <= d_end <= ref_period_end
            # [maybe the equality should be enforced, since
            # ref_period_start < d_start <= d_end < ref_period_end
            # could give wrong results] ???
            return period * daycount(c,d_start, d_end) / daycount(c,ref_period_start, ref_period_end)
        else
            # here ref_period_start is the next (maybe notional)
            # payment date and ref_period_end is the second next
            # (maybe notional) payment date.
            # d_start < ref_period_start < ref_period_end
            # AND d_end <= ref_period_end
            # this case is long first coupon

            # the last notional payment date
            previousRef = ref_period_start + months(-mnths)
            if (d_end>ref_period_start)
                return yearfraction(c, d_start, ref_period_start, previousRef,ref_period_start) + yearfraction(c,ref_period_start, d_end, ref_period_start, ref_period_end)
            else
                return yearfraction(d_start, d_end, previousRef,ref_period_start)
            end
        end
    else
        # here ref_period_end is the last notional payment date
        # d_start < ref_period_end < d_end AND ref_period_start < ref_period_end
        @assert ref_period_start<=d_start
        # now it is: ref_period_start <= d_start < ref_period_end < d_end

        # the part from d_start to ref_period_end
         sum = yearfraction(c,d_start, ref_period_end, ref_period_start, ref_period_end)

        # the part from ref_period_end to d_end
        # count how many regular periods are in [ref_period_end, d_end],
        # then add the remaining time
        i = 0
        local newRefStart, newRefEnd
        while true
            newRefStart = ref_period_end + months(months * i)
            newRefEnd = ref_period_end+ months(months * (i + 1))
            if (d_end<newRefEnd)
                break
            else
                sum += period
                i += 1
            end
        end
        sum += yearFraction(c, newRefStart, d_end, newRefStart, newRefEnd)
        return sum
    end
end

yearfraction(c::BusinessDay252, d_start::CalendarTime, d_end::CalendarTime) = daycount(c, d_start, d_end) / 252
