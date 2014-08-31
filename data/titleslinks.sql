SELECT page_title, pl_title
FROM  `pagelinks` 
INNER JOIN  `page` ON pagelinks.pl_from = page.page_id
WHERE page_namespace =0
AND pl_namespace =0
ORDER BY page_title


/* number of links total :                         7,363,585 */
/* number of links to a 0-page :                   5,927,539 */
/* number of links from a 0-page :                 5,671,064 */
/* number of links from a 0-page to a non-0-page :   231,143 */
/* number of links from a non-0 page to a 0-page :   487,618 */
/* number of links from a 0-page to a 0-page :     5,439,921 */
/* everything checks out */

/* number of pages total : 324,815 */
/* number of 0-pages :     146,017 */
/* links per 0-page :          ~37 */

/*BAD!! number of links without redirects taken into account :    5,396,808 */
/*BAD!! non redirect pages :         103,225 */

/* unique src: 145,598 */
/* unique dst: 668,112 */
/* unique both: 696,271 */
/* but unique 0-pages : 146,017 this is a mystery why this occurs but i'm ignoring it */
