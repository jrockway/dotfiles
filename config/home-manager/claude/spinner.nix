# Spinner customization, merged into ~/.claude/settings.json alongside
# permissions.nix and auto-mode.nix (see home.activation.claudeMergeSettings in
# home.nix).
#
# Both keys are objects, not bare arrays. A bare array here makes Claude Code
# reject ~/.claude/settings.json wholesale at startup ("spinnerVerbs: Expected
# object") — permissions, hooks and all.
#
# Fall 2026 outfit, chosen by the dog.
{
  # What the spinner says while Claude works. "replace" drops the stock verbs
  # entirely; "append" would keep them in the rotation alongside these.
  spinnerVerbs = {
    mode = "replace";
    verbs = [
      "Fetching"
      "Sniffing"
      "Digging"
      "Barking"
      "Wagging"
      "Herding"
      "Pawing at"
      "Zooming"
      "Trotting"
      "Scenting"
      "Nosing around"
      "Burying bones"
      "Chasing squirrels"
      "Rustling leaves"
      "Rolling in leaves"
      "Bringing it back"
      "Circling before settling"
      "Howling at the harvest moon"
    ];
  };

  # A few tips appended to the stock rotation. excludeDefault = true would
  # replace the built-in tips instead of adding to them.
  spinnerTipsOverride = {
    tips = [
      "Bark! means the smoking gun. Bark! Bark! means it's confirmed."
      "jj st before jj new: @ is often already on the target commit."
      "Honeycomb result JSON links are curl-able for ~15 minutes: exact data for charts."
    ];
  };
}
