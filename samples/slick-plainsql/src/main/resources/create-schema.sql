create table suppliers(
  id int not null primary key,
  name varchar not null,
  street varchar not null,
  city varchar not null,
  state varchar not null,
  zip varchar not null);

insert into suppliers values(101, 'Acme, Inc.', '99 Market Street', 'Groundsville', 'CA', '95199');
insert into suppliers values(49, 'Superior Coffee', '1 Party Place', 'Mendocino', 'CA', '95460');
insert into suppliers values(150, 'The High Ground', '100 Coffee Lane', 'Meadows', 'CA', '93966');
