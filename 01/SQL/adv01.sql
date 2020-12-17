-- This script is only compatible with POSTGRES
-- The "Solution" uses vanila sql, but \copy is used to read file into table
DROP TABLE IF EXISTS advent01;
CREATE TABLE advent01(
    expense INT PRIMARY KEY NOT NULL
);

\copy advent01 FROM '../input'

\echo PART 1
SELECT T1.expense, T2.expense, T1.expense * T2.expense as solution
    FROM
        advent01 as T1
    CROSS JOIN
        advent01 as T2
    WHERE
        T1.expense + T2.expense = 2020
    LIMIT 1;

\echo PART 2
SELECT T1.expense, T2.expense, T3.expense, T1.expense * T2.expense * T3.expense as solution
    FROM
        advent01 as T1
    CROSS JOIN
        advent01 as T2
    CROSS JOIN
        advent01 as T3
    WHERE
        T1.expense + T2.expense + T3.expense = 2020
    LIMIT 1;

        
    