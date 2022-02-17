def filter(fn): map(select(fn));

def simple_array:
  .[1:-1]
  | split(",")
  ;

def extract_name:
  split(".") | .[-1]
  ;

def contains_any($args):
  . as $input
  | $args
  | [
    .[]
    | . as $arg
    | $input
    | contains($arg)
    ]
  | any
  ;

def convert_to_rate($str; $duration):
  $str
  | split("),")
  | map(
    capture("ItemClass=[^.]+\\.(?<item>[A-z_]+)\"_,Amount=(?<amount>\\d+)"; "g")
    | (.amount |= tonumber)
    | (. + { rate: (60 / $duration * .amount) })
  )
  ;

def add_rate($parent; $arr):
  $arr
  | map(. + { rate: (60 / $parent.duration * .amount) })
  ;

def in($arr):
  . as $in
  | $arr
  | map({ (.): true })
  | add
  | .[$in] // false
  ;

def to_recipe:
  (.mManufactoringDuration | tonumber) as $duration
  | {
      id: .ClassName,
      name: .mDisplayName,
      inputs: convert_to_rate(.mIngredients; $duration),
      outputs: convert_to_rate(.mProduct; $duration),
      duration: $duration,
      producer: .mProducedIn | simple_array | map(extract_name),
    }
  ;

def to_item:
  {
    id: .ClassName,
    name: .mDisplayName,
    short_name: (.mAbbreviatedDisplayName // .mDisplayName)
  };


