#pragma once

#include "rust/cxx.h"
#include "runix-ffi/src/lib.rs.h"

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


} // namespace runix
