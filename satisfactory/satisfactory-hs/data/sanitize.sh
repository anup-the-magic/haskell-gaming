#!/usr/bin/env bash
set -euC -o pipefail

function usage() {
  echo "$0 <output-folder>"
}

cd "$(dirname "$0")"


FILE='./Docs.json'

JSON=$(\
cat "$FILE" \
  | iconv -f UTF16 -t UTF-8 -c \
  | sed "s/'/_/g"\
)

function get() {
  echo "$JSON" \
    | jq '
      .[]
      | select(
        .NativeClass[:-1]
        | split(".")[-1]
        | (. == "'$1'")
      )
      | .Classes
      | .[]
    '
}

function header() {
  echo -e "\n============== $1 ==============\n"
}

function debug() {
  jq -s "${1:-.[0]}" \
    | tee >(jq >&2)
}


# echo "$JSON" | jq '.[5] | .NativeClass'

# | debug 'map(.mDisplayName, .mIngredients)' \
# | debug 'map(.mProduct | match("\\.([a-zA-Z_]+)\""; "g")) | .[]'\

# echo $JSON \
#   | debug '
#   include "./helpers";
#   .[]
#   | map(
#     {
#       (.NativeClass[1:-1] | split(".")[-1]): [
#         .Classes
#         | map(.ClassName | select(contains("Water")))
#       ]
#     }
#   )
#   | add
#   '

items=$(\
  get "FGItemDescriptor" \
  | jq -s '
  include "./helpers";
  map(to_item)
  '
)

recipes=$(\
  get "FGRecipe" \
  | jq -s '
  include "./helpers";
  map(
    to_recipe
    | select(
        .producer
        | any(
            in([
              "Build_Converter_C",
              "Build_AssemblerMk1_C",
              "Build_Blender_C",
              "Build_ConstructorMk1_C",
              "Build_FoundryMk1_C",
              "Build_HadronCollider_C",
              "Build_ManufacturerMk1_C",
              "Build_OilRefinery_C",
              "Build_Packager_C",
              "Build_SmelterMk1_C",
              "Build_AutomatedWorkBench_C",
              "BP_WorkshopComponent_C"
            ])
        )
    )
    | select(
        (.producer | any(. != "BP_WorkshopComponent_C"))
        or
        (.producer | all(. != "BP_WorkshopComponent_C"))
      )
  )
  '
)

buildables=$(\
  get "FGRecipe" |
  jq -s '
  include "./helpers";
  map(
    select(
        .mProducedIn
        | simple_array
        | map (extract_name)
        | any(
          .
          | in([
              "BP_WorkshopComponent_C",
              "FGBuildGun",
              "BP_BuildGun_C"
            ])
          )
    )
    | to_recipe
    | select(
        (.producer | all(. == "BP_WorkshopComponent_C"))
        or
        (.producer | any(. == "BP_WorkshopComponent_C") | not)
    )
  )
  '
)

output=$(
  header "Items"
  echo $items | jq -C '.[0]'
  header "Recipes"
  echo $recipes | jq -C '.[0]'
  header "buildables"
  echo $recipes | jq -C '.[0]'
)
header "OUTPUT" && echo "$output"

OUTPUT_DIR=${1-}
if [[ ! -z $OUTPUT_DIR ]]; then
  # if [[ $OUTPUT_DIR -eq "--help" ]]; then usage && exit 0; fi

  echo "Writing output"

  mkdir $OUTPUT_DIR
  echo $items | jq > $OUTPUT_DIR/items.json
  echo $recipes | jq > $OUTPUT_DIR/recipes.json
  echo $buildables | jq > $OUTPUT_DIR/buildables.json
fi
