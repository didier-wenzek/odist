type 'a col
type ('a,'b,'c) red
type 'a monoid = ('a,'a,'a) red
type 'a option_monoid = ('a, 'a option, 'a option) red
type ('a,'b) col_monoid = ('a, 'b, 'a col) red

val reduce: ('a,'b,'c) red -> 'a col -> 'c
val fold: ('a -> 'b) -> ('b,'c,'d) red -> 'a col -> 'd

val map: ('a -> 'b) -> 'a col -> 'b col
val flatmap: ('a -> 'b col) -> 'a col -> 'b col
val unnest: ('a -> 'b col) -> 'a col -> ('a*'b) col
val filter: ('a -> bool) -> 'a col -> 'a col
val col_product: 'a col -> 'b col -> ('a -> 'b -> 'c) -> 'c col

val monoid: 'a -> ('a -> 'a -> 'a) -> 'a monoid
val opt_monoid: ('a -> 'a -> 'a) -> 'a option_monoid
val col_monoid: 'a -> ('a -> 'b -> 'a) -> ('a -> 'a -> 'a) -> ('a -> 'c) -> ('b, 'a, 'c) red
val with_absorber: 'b -> ('a,'b,'c) red -> ('a,'b,'c) red

val red_map: ('a -> 'b) -> ('b,'c,'d) red -> ('a,'c,'d) red
val red_flatmap: ('a -> 'b col) -> ('b,'c,'d) red -> ('a,'c,'d) red
val red_unnest: ('a -> 'b col) -> ('a*'b,'c,'d) red -> ('a,'c,'d) red
val red_filter: ('a -> bool) -> ('a,'b,'c) red -> ('a,'b,'c) red
val red_product: ('a,'b,'c) red -> ('a,'d,'e) red -> ('c -> 'e -> 'f) -> ('a, 'b*'d, 'f) red

val list: 'a list -> 'a col
val range: int -> int -> int col
val list_reducer: ('a, 'a list, 'a col) red
