#pragma once

#include "rust/cxx.h"
#include "runix-ffi/src/lib.rs.h"

#include <nix/flake/flake.hh>
#include <nix/fetchers.hh>
#include <nix/error.hh>

namespace rust
{
    namespace behavior
    {
        template <typename Try, typename Fail>
        static void trycatch(Try &&func, Fail &&fail) noexcept
        try
        {
            func();
        }
        catch (nix::BaseError &e)
        {
            fail(e.info().msg.str());
        }
        catch (const std::exception &e)
        {
            fail(e.what());
        }
    }
}

namespace runix
{

    rust::String parse_flakeref(rust::Str url);

} // namespace runix
