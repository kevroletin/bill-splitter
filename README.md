# bill-splitter

I wrote this script on my knee, ideally, it should be a web app or a mobile app.

It helps in a situation when a group of people was consuming some products or
services. But each item was consumed by different subset of people. And payment
was made by several different people.

Below is an example: man1 purchased beer and a pizza and invited his friends
man2 and man3 to a party. They consumed a pizza but man 2 didn't drink alcohol.
And man3 also purchased another pizza. After a party, man2 and man3 took a taxi
and man2 paid for it.

```
|       | man1 IN | man1 OUT | man2 IN | man2 OUT | man3 IN | man3 Out |
|-------|---------|----------|---------|----------|---------|----------|
| beer  | +       |       50 |         |          | +       |          |
| pizza | +       |       50 | +       |          | +       |       50 |
| taxi  |         |          | +       |       25 | +       |          |
```

This script will produces next calculations. Now it's clear that man2 and man3
owe sama amount of money to a man1:

```
|         | man1 IN | man1 OUT | man2 IN | man2 OUT | man3 IN | man3 OUT |
|---------|---------|----------|---------|----------|---------|----------|
| beer    |      25 |       50 |         |          |      25 |          |
| pizza   |    33.3 |       50 |    33.3 |          |    33.3 |       50 |
| taxi    |         |          |    12.5 |       25 |    12.5 |          |
| summary |    58.3 |      100 |    45.8 |       25 |    70.8 |       50 |
 
+ man1 :: -41.7
+ man2 :: 20.8
+ man3 :: 20.8
```

Run like this. It will produce unformatted org table which is supposed to be
beautified by emacs org-mode.

```
stack build && stack exec bill-splitter < example.org
```
