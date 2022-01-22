def convert_to_rate($str):
  $str
  | split("),")
  | map(
    capture("ItemClass=[^.]+\\.(?<item>[A-z_]+)\"_,Amount=(?<amount>\\d+)"; "g")
    | (.amount |= tonumber)
  )
  ;

def add_rate($parent; $arr):
  $arr
  | map(. + { rate: (60 / $parent.duration * .amount) })
  ;
