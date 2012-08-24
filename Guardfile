
guard :shell, all_on_start: true do
  watch  /Taygeta.hs|((Taygeta|Test|Text)\/.*\.l?hs)$/  do |m|
    puts "\n\n\nCompiling Taygeta..."
    `rm -rf test-taygeta.tix`
    `cabal build && ./dist_taygeta/build/test-taygeta/test-taygeta`
  end
  
  watch /scratch\/LDA.hs/ do |m|
    puts "\n\n\nCompiling LDA..."
    `ghc --make -rtsopts -threaded scratch/LDA.hs && ./scratch/LDA`
  end
end

