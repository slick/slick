drop table coffees;
create table coffees(id int auto_increment, name varchar(255), supid int, price double);
insert into coffees(name, supid, price) values ('Colombian', 101, 7.99);
insert into coffees(name, supid, price) values ('Colombian_Decaf', 101, 8.99);
insert into coffees(name, supid, price) values ('French_Roast_Decaf', 49, 9.99);
select * from coffees