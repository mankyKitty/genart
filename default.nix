{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
let
  sources = {
    reflex-dom-canvas = import ./nix/reflex-dom-canvas.nix;
    reflex-dom-svg = import ./nix/reflex-dom-svg.nix;
  };
in
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ ... }: {

  android = null;
  ios = null;

  overrides = self: super: {

    reflex-dom-canvas = self.callPackage sources.reflex-dom-canvas {};
    reflex-dom-svg = self.callPackage sources.reflex-dom-svg {};

  };

  # android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  # android.displayName = "Obelisk Minimal Example";
  # ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  # ios.bundleName = "Obelisk Minimal Example";
})
