create table "a" ("k1" INTEGER NOT NULL,"k2" INTEGER NOT NULL,"s" VARCHAR NOT NULL);
create table "b" ("f1" INTEGER NOT NULL,"f2" INTEGER NOT NULL,"s" VARCHAR NOT NULL);
create unique index "b_idx1" on "b" ("f1","f2");
alter table "a" add constraint "b_fk" foreign key("k1","k2") references "b"("f1","f2") on update NO ACTION on delete CASCADE;