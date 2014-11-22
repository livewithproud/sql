drop view if exists period;
drop view if exists period_n;

create view period as
select distinct (to_days(time) -735689) div 7 as period, userId from analyzeuser; # -((735810-735689) mod 7)

create view period_n as
select userId, count(*) as n
from period
group by userId;

create view user as
select userId
from period_n
where n = (735810-735689) div 7
limit 10;

select itemName from analyzeuser where userId in (select * from user);

drop view period;
drop view period_n;

