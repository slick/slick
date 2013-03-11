create table "a" ("k1" INTEGER NOT NULL,"k2" INTEGER NOT NULL,"s" VARCHAR NOT NULL);
create table "b" ("f1" INTEGER NOT NULL,"f2" INTEGER NOT NULL,"s" VARCHAR NOT NULL);
alter table "a" add constraint "b_fk" foreign key("k1") references "b"("f1") on update NO ACTION on delete CASCADE;