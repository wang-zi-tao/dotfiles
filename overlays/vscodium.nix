final: prev: {
  vscodium = prev.vscodium.overrideAttrs (oldAttrs: {
    postInstall = ''
      ${final.jq}/bin/jq --unbuffered '. + {"extensionsGallery": { "serviceUrl": "https://marketplace.visualstudio.com/_apis/public/gallery", "itemUrl": "https://marketplace.visualstudio.com/items" }}' > tee $out/lib/vscode/resources/app/product.json 
      cat $out/lib/vscode/resources/app/product.json
    '';
  });
}
