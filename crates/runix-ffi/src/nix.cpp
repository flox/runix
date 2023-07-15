
#include "runix-ffi/include/nix.h"

#include <string.h>
#include <nlohmann/json.hpp>

#include <nix/attrs.hh>

using namespace std;

namespace runix
{
    rust::String parse_flakeref(rust::Str url)
    {
        nix::FlakeRef originalRef = nix::parseFlakeRef(string(url), nix::absPath("."));
        nix::fetchers::Attrs attrs = originalRef.toAttrs();
        nlohmann::json flakerefJson = nix::fetchers::attrsToJSON(attrs);
        return flakerefJson.dump();
    }
}
