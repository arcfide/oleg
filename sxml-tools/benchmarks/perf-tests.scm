;; Performance tests for DDO SXPath

(define perf-tests
  `("site/people/person[@id='person0']/name/text()"
    "site/open_auctions/open_auction/bidder[1]/increase/text()"
    "site/open_auctions/open_auction[bidder[1]/increase/text() *2 <= bidder[last()]/increase/text()]"
    "site/open_auctions/open_auction[bidder[personref[@person='person1']]/following-sibling::bidder/personref[@person='person4']]/reserve/text()"
    "site/closed_auctions/closed_auction[price/text() >= 40]/price"
    "count(site/regions//item)"
    "count(site//description) + count(site//annotation) + count(site//email)"
    "site/people/person[/site/closed_auctions/closed_auction/buyer/@person = ./@id]/name/text()"
    "site/people/person[@id = /site/closed_auctions/closed_auction[itemref/@item = /site/regions/europe/item/@id]/buyer/@person]/name/text()"
    "site/people/person[profile/interest/@category = /site/people/person/profile/interest/@category]/profile/education"
    "site/people/person[profile/@income > (5000 * /site/open_auctions/open_auction/initial/text())]/name/text()"
    "site/people/person[profile/@income > (5000 * /site/open_auctions/open_auction/initial/text())][profile/@income > 50000]/name/text()"
    "site/regions/australia/item/description"
    "site//item[contains(description, 'gold')]/name/text()"
    "site/closed_auctions/closed_auction/annotation/description/parlist/listitem/parlist/listitem/text/emph/keyword/text()"
    "site/closed_auctions/closed_auction[annotation/description/parlist/listitem/parlist/listitem/text/emph/keyword/text()]/seller/@person"
    "site/people/person[not(homepage/text())]/name/text()"
    "site/regions//item/name/text()"
    "count(site/people/person/profile[@income < 100000 and @income >= 30000])"
    "/site//item"
    "site/closed_auctions/closed_auction/annotation"
    "site/*/person"
    "site/regions/*/item/name"
    "/descendant::item[count(descendant::parlist/child::listitem)+position() < last()]/child::location"
    "//*[@category]"
    "//*[date and time]"
    "site/regions//listitem"
    "site//parlist/listitem"
    "/site/regions/*[self::europe or self::asia]"
    "//parlist//listitem"))

(define doc
  (sxml:document "http://www.modis.ispras.ru/Lizorkin/XML/auction1.xml"))
(display "Document obtained")
(newline)
(newline)

(let loop ((lpaths perf-tests)
           (n 1))
  (if
   (null? lpaths)
   #t
   (let ((impl-ddo (ddo:xpath-expr (car lpaths))))
     (display "Test No. ")
     (display n)
     (newline)
     (display (car lpaths))
     (newline)
     (let ((res-pair
            (sxml:time-apply impl-ddo (list doc))))
       (if
        (nodeset? (car res-pair))
        (begin
          (display "Number of nodes selected: ")
          (display (length (car res-pair))))
        (begin
          (display "Result: ")
          (display (car res-pair))))
       (newline)
       (display "Execution time, msec: ")       
       (display (cdr res-pair))
       (newline)
       (newline)
       (loop (cdr lpaths) (+ n 1))
       ))))
