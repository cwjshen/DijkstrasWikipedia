/*SELECT title
FROM `uniquesrc`
union
select title
from `uniquedst`
order by title */

SELECT page_title AS title
FROM edges
UNION 
SELECT pl_title AS title
FROM edges
ORDER BY title
