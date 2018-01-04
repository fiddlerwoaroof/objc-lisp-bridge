#include <Foundation/NSException.h>
#define EXPORT __attribute__((visibility("default")))

EXPORT void set_uncaught_exception_handler(NSUncaughtExceptionHandler * _Nullable handler) {
  NSSetUncaughtExceptionHandler(handler);
};
