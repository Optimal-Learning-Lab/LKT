param([Parameter(Mandatory=$true)][string]$Root,
      [Parameter(Mandatory=$true)][string]$Artifact,
      [Parameter(Mandatory=$true)][string]$CandidateDirectory,
      [Parameter(Mandatory=$true)][string]$Reference,
      [Parameter(Mandatory=$true)][string]$ExistingRemoteInputSpec,
      [Parameter(Mandatory=$true)][string]$RemoteReferenceDirectory)
$ErrorActionPreference='Stop'
$bundle=Join-Path $Artifact 'code-only-bundle'
if(Test-Path -LiteralPath $bundle){throw 'Code-only bundle already exists.'}
New-Item -ItemType Directory -Path "$bundle/scripts/hpc","$bundle/tests","$bundle/candidate","$bundle/package-source/LKT" | Out-Null
foreach($name in @('compare-gru-numeric-ablation.R','report-gru-numeric-ablation.R','verify-gru-numeric-ablation-results.R','gru-encoder-study.R','prepare-gru-simplified-remote.py')) {
  Copy-Item -LiteralPath "$Root/scripts/$name" -Destination "$bundle/scripts/$name"
}
foreach($name in @('setup','run','report')) {
  $text=[IO.File]::ReadAllText("$Root/scripts/hpc/$name-gru-numeric-ablation.slurm").Replace("`r`n","`n")
  [IO.File]::WriteAllText("$bundle/scripts/hpc/$name-gru-numeric-ablation.slurm",$text,[Text.UTF8Encoding]::new($false))
}
foreach($name in @('gru-defaults.R','gru-simplified.R','gru-numeric-selection.R')) {
  Copy-Item -LiteralPath "$Root/tests/$name" -Destination "$bundle/tests/$name"
}
$checks=Import-Csv -LiteralPath "$CandidateDirectory/source-checksums.csv"
$packageHashes=[ordered]@{}
foreach($row in $checks) {
  $packageHashes[$row.path]=$row.sha256
  if($row.path.StartsWith('data/')){continue}
  $source=Join-Path "$CandidateDirectory/LKT" $row.path
  if([IO.Path]::GetExtension($source) -match '^\.(rds|rda|RData|csv|tsv|parquet|zip|gz)$'){throw "Non-source input excluded from upload: $($row.path)"}
  $destination=Join-Path "$bundle/package-source/LKT" $row.path
  New-Item -ItemType Directory -Path (Split-Path $destination -Parent) -Force | Out-Null
  Copy-Item -LiteralPath $source -Destination $destination
}
$referenceHashes=[ordered]@{}
$names=@('study.rds','data/prepared_data.rds')
foreach($part in @('cv1','cv2','cv3','cv4','cv5')) {
  $names+="partitions/$part.rds"
  foreach($seed in 1..3){$names+="models/${part}__simplified__seed$seed.rds"}
}
foreach($name in $names) {
  $referenceHashes[$name]=(Get-FileHash -LiteralPath (Join-Path $Reference $name) -Algorithm SHA256).Hash.ToLowerInvariant()
}
$existing=Get-Content -LiteralPath $ExistingRemoteInputSpec -Raw | ConvertFrom-Json
$spec=[ordered]@{reference_directory=$RemoteReferenceDirectory;
  reference_hashes=$referenceHashes;package_source_hashes=$packageHashes;
  existing_source_archive=$existing.existing_source_archive;
  existing_source_archive_sha256=$existing.existing_source_archive_sha256}
$spec | ConvertTo-Json -Depth 8 | Set-Content -LiteralPath "$bundle/remote-inputs.json" -Encoding utf8NoBOM
$meta=Get-Content -LiteralPath "$CandidateDirectory/candidate.json" -Raw | ConvertFrom-Json
$meta | Add-Member -NotePropertyName windows_verified_source_archive_sha256 -NotePropertyValue $meta.source_sha256
$meta.PSObject.Properties.Remove('source_sha256')
$meta | ConvertTo-Json -Depth 8 | Set-Content -LiteralPath "$bundle/candidate/candidate-template.json" -Encoding utf8NoBOM
$files=Get-ChildItem -LiteralPath $bundle -Recurse -File | Sort-Object FullName
foreach($file in $files) {
  if($file.Extension -match '^\.(rds|rda|RData|csv|tsv|parquet)$'){throw "Data file unexpectedly staged: $($file.FullName)"}
}
$lines=$files | ForEach-Object {
  $relative=[IO.Path]::GetRelativePath($bundle,$_.FullName).Replace('\','/')
  (Get-FileHash -LiteralPath $_.FullName -Algorithm SHA256).Hash.ToLowerInvariant()+'  '+$relative
}
[IO.File]::WriteAllText("$bundle/bundle.sha256",($lines -join "`n")+"`n",[Text.UTF8Encoding]::new($false))
& tar -czf "$Artifact/gru-numeric-ablation-code-only.tar.gz" -C $bundle .
if($LASTEXITCODE -ne 0){throw 'Code-only archive failed'}
$hash=(Get-FileHash -LiteralPath "$Artifact/gru-numeric-ablation-code-only.tar.gz" -Algorithm SHA256).Hash.ToLowerInvariant()
[IO.File]::WriteAllText("$Artifact/code-only-upload.sha256","$hash  gru-numeric-ablation-code-only.tar.gz`n",[Text.UTF8Encoding]::new($false))
Write-Output "Code-only source files: $($files.Count); archive SHA256: $hash"
