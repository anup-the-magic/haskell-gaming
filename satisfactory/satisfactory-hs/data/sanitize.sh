#!/usr/bin/env bash
set -euC -o pipefail

function usage() {
  echo "$0 <output-folder>"
}

cd "$(dirname "$0")"

OUTPUT_DIR=${1?$(usage)}

FILE='./Docs.json'

JSON=$(\
cat "$FILE" \
  | iconv -f UTF16 -t UTF-8 -c \
  | sed "s/'/_/g"\
)

function get() {
  echo "$JSON" \
    | jq '
      .[] | select(.NativeClass | contains("FactoryGame.'$1'_"))
      | .Classes | .[]'
}

function header() {
  echo -e "\n\n==============$1==============\n\n"
}

function debug() {
  tee >(jq -s "${1:-.[0]}" >&2)
}


# echo "$JSON" | jq '.[5] | .NativeClass'

header "ITEMS"

items=$(\
  get "FGItemDescriptor" \
  | jq 'with_entries(select(.value != ""))' \
  | jq '{
    id: .ClassName,
    name: .mDisplayName,
    short_name: (.mAbbreviatedDisplayName // .mDisplayName),
  }' \
  | jq -s . \
)

header "RECIPES"

# | debug 'map(.mDisplayName, .mIngredients)' \
# | debug 'map(.mProduct | match("\\.([a-zA-Z_]+)\""; "g")) | .[]'\

recipes=$(\
  get "FGRecipe" \
  | jq -s 'include "./helpers";
  map({
    id: .ClassName,
    name: .mDisplayName,
    inputs: convert_to_rate(.mIngredients),
    outputs: convert_to_rate(.mProduct),
    duration: .mManufactoringDuration | tonumber,
  }
  | select(.inputs | length > 1)
  | (.inputs = add_rate(.; .inputs))
  | (.outputs = add_rate(.; .outputs))
  )
' \
)

header "OUTPUT"
echo Items
echo $items | jq '.[0]'
echo
echo Recipes
echo $recipes | jq '.[0]'

mkdir $OUTPUT_DIR
echo $items > $OUTPUT_DIR/items.json
echo $recipes > $OUTPUT_DIR/recipes.json
