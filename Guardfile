
guard :shell, all_on_start: true do
  watch  /(Taygeta|Test|Text)\/.*\.l?hs$/  do |m|
    puts "\n\n\nCompiling..."
    `rm -rf test-taygeta.tix`
    `cabal build && ./dist_taygeta/build/test-taygeta/test-taygeta`
  end
end

