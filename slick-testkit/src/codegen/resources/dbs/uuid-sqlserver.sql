create table "person" ("id" INTEGER NOT NULL PRIMARY KEY,
                       "uuid" UNIQUEIDENTIFIER NOT NULL,
                       "uuid_def" UNIQUEIDENTIFIER DEFAULT('2f3f866c-d8e6-11e2-bb56-50e549c9b654'),
                       "uuid_func" UNIQUEIDENTIFIER DEFAULT NEWID()
                      );
