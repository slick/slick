const pathParts = window.location.pathname.split('/')
const versionIndex = pathParts.findIndex(s => s !== '' && s !== 'doc')
const currentVersion = pathParts[versionIndex]
const versionsJsonPath = pathParts.slice(0, versionIndex).join('/') + '/versions.json'
fetch(versionsJsonPath)
  .then(response => response.json())
  .then(data => data.reverse())
  .then(allVersions => {
    for (const menu of document.querySelectorAll(".version-menu")) {
      const parent = menu.querySelector('ul.menu')
      for (const v of allVersions) {
        if (v !== currentVersion) {
          pathParts[versionIndex] = v
          parent.insertAdjacentHTML("beforeend", `<li><a href="${pathParts.join("/")}">${v}</a></li>`)
        }
      }
      menu.classList.add('dropdown')
      menu.setAttribute("data-dropdown-menu", "")
      $(menu).foundation()
    }
  });
