create table scores(
  username text not null,
  year int not null,
  day int not null,
  environment text not null,
  test text not null,
  time int not null,
  primary key (username, year, day, environment, test)
);
