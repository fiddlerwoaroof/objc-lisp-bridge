#include <stdio.h>

#include <Foundation/NSException.h>
#include <Foundation/NSGeometry.h>
#include <AppKit/NSWindow.h>
#define EXPORT __attribute__((visibility("default")))

EXPORT void set_uncaught_exception_handler(NSUncaughtExceptionHandler * _Nullable handler) {
  NSSetUncaughtExceptionHandler(handler);};

EXPORT void printRect(NSRect rect) {
  printf("Got a rect: (%f %f), (%f %f)\n",
         rect.size.width,
         rect.size.height,
         rect.origin.x,
         rect.origin.y);}

EXPORT id initWindow(NSWindow *window, NSRect *rect, char a, char b, Boolean c) {
  printf("Got a rect: (%f %f), (%f %f)\n", rect->size.width, rect->size.height, rect->origin.x, rect->origin.y);
  return [window initWithContentRect: *rect
                           styleMask: a
                             backing: b
                               defer: c];}
