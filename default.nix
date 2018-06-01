{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ ... }: {

  overrides = self: super: let
    rfx-canvas = import ./nix/reflex-dom-canvas.nix;
    rfx-svg = import ./nix/reflex-dom-svg.nix;
  in
  {
    reflex-dom-canvas = self.callCabal2nix "reflex-dom-canvas" rfx-canvas {};
    reflex-dom-svg = self.callCabal2nix "reflex-dom-svg" rfx-svg {};
  };

  # Don't want any mobile builds just yet.
  android = null;
  ios = null;

  # android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  # android.displayName = "Obelisk Minimal Example";
  # ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  # ios.bundleName = "Obelisk Minimal Example";
})
